module Solcore.Backend.EmitHull (emitHull) where

import Common.Monad
import Control.Monad (when)
import Control.Monad.State
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String (fromString)
import GHC.Stack (HasCallStack)
import Language.Hull qualified as Hull
import Language.Yul
import Solcore.Backend.Mast
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax.Contract (Constr (..), DataTy (..))
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt (Literal (..))
import Solcore.Frontend.Syntax.Ty (Ty (..), Tyvar (..))
import Solcore.Frontend.TypeInference.TcMonad (insts)
import Prelude hiding (product)

emitHull :: Bool -> MastCompUnit -> IO [Hull.Object]
emitHull debugp cu = fmap concat $ runEM debugp $ mapM emitTopDecl (mastTopDecls cu)

type EM a = StateT EcState IO a

runEM :: Bool -> EM a -> IO a
runEM debugp m = evalStateT m (initEcState debugp)

errorsEM :: (HasCallStack) => [String] -> EM a
errorsEM msgs = do
  writes ("\nERROR: " : msgs)
  context <- gets ecContext
  let contextStr = unlines (map ("in: " ++) context)
  writeln contextStr
  error "Emit hull failed"

data EcState = EcState
  { ecSubst :: VSubst,
    ecDT :: DataTable,
    ecNest :: Int,
    ecDebug :: Bool,
    ecContext :: [String],
    ecDeployer :: Maybe Hull.Body,
    ecFresh :: Int
  }

initEcState :: Bool -> EcState
initEcState debugp =
  EcState
    { ecSubst = emptyVSubst,
      ecDT = Map.fromList builtinDataInfo,
      ecNest = 0,
      ecDebug = debugp,
      ecContext = [],
      ecDeployer = Nothing,
      ecFresh = 0
    }

-- isolate local changes to nesting level and variable substitution
-- e.g. when translating match alts
withLocalState :: EM a -> EM a
withLocalState m = do
  s <- get
  a <- m
  s' <- get
  put s' {ecNest = ecNest s, ecSubst = ecSubst s}
  return a

freshInt :: EM Int
freshInt = state (\s -> (ecFresh s, s {ecFresh = ecFresh s + 1}))

type DataTable = Map.Map Name DataTy

sumDataTy :: DataTy
sumDataTy =
  DataTy
    { dataName = "sum",
      dataParams = [TVar "a", TVar "b"],
      dataConstrs =
        [ Constr "inl" [tyvar "a"],
          Constr "inr" [tyvar "b"]
        ]
    }
  where
    tyvar = TyVar . TVar

builtinDataInfo :: [(Name, DataTy)]
builtinDataInfo = [("sum", sumDataTy)]

type VSubst = Map.Map Name Hull.Expr

emptyVSubst :: VSubst
emptyVSubst = Map.empty

extendVSubst :: VSubst -> EM ()
extendVSubst subst = modify extend
  where
    extend s = s {ecSubst = ecSubst s <> subst}

pushContext :: String -> EM ()
pushContext c = modify extend
  where
    extend s = s {ecContext = c : ecContext s}

dropContext :: EM ()
dropContext = modify (\s -> s {ecContext = drop 1 $ ecContext s})

withContext :: String -> EM a -> EM a
withContext s m = pushContext s *> m <* dropContext

type Translation a = EM (a, [Hull.Stmt])

emitTopDecl :: MastTopDecl -> EM [Hull.Object]
emitTopDecl (MastTContr c) = withLocalState do
  runtimeObj <- emitContract c
  pure [runtimeObj]
emitTopDecl (MastTDataDef dt) = addData dt >> pure []

addData :: DataTy -> EM ()
addData dt = modify (\s -> s {ecDT = Map.insert (dataName dt) dt (ecDT s)})

emitContract :: MastContract -> EM Hull.Object
emitContract c = do
  let cname = show (mastContrName c)
  writes ["Emitting hull for contract ", cname]
  runtimeBody <- concatMapM emitCDecl (mastContrDecls c)
  deployer <- gets ecDeployer
  case deployer of
    Nothing -> pure (Hull.Object cname runtimeBody [])
    Just code ->
      let runtimeObject = Hull.Object cname runtimeBody []
       in pure (Hull.Object (cname ++ "Deploy") code [runtimeObject])

emitCDecl :: MastContractDecl -> EM [Hull.Stmt]
emitCDecl (MastCFunDecl f) = emitFunDef f
emitCDecl (MastCMutualDecl ds) = case findConstructor ds of
  Nothing -> do
    body <- concatMapM emitCDecl ds
    pure [Hull.SBlock body]
  Just _ -> do
    -- this is the deployer block
    depDecls <- concatMapM emitCDecl ds
    modify (\s -> s {ecDeployer = Just depDecls})
    pure []
emitCDecl (MastCDataDecl dt) = addData dt >> pure []

-- look up the deployer start routine
findConstructor :: [MastContractDecl] -> Maybe MastFunDef
findConstructor = go
  where
    go [] = Nothing
    go (MastCFunDecl d : _) | mastFunName d == "start" = Just d
    go (_ : ds) = go ds

-----------------------------------------------------------------------
-- Translating function definitions
-----------------------------------------------------------------------
emitFunDef :: (HasCallStack) => MastFunDef -> EM [Hull.Stmt]
emitFunDef fd = withContext (show (mastFunName fd)) do
  let name = show (mastFunName fd)
  hullArgs <- mapM translateParam (mastFunParams fd)
  hullTyp <- translateMastType (mastFunReturn fd)
  debug ["\n# emitFunDef ", name, " :: ", show hullTyp]
  hullBody <- emitStmts (mastFunBody fd)
  let hullFun = Hull.SFunction name hullArgs hullTyp hullBody
  dropContext
  return [hullFun]

translateParam :: MastParam -> EM Hull.Arg
translateParam (MastParam n t) = Hull.TArg (show n) <$> translateMastType t

-----------------------------------------------------------------------
-- Translating types and value constructors
-----------------------------------------------------------------------

translateMastType :: (HasCallStack) => MastTy -> EM Hull.Type
translateMastType (MastTyCon "word" []) = pure Hull.TWord
translateMastType (MastTyCon "unit" []) = pure Hull.TUnit
translateMastType (MastTyCon "()" []) = pure Hull.TUnit
translateMastType t@(MastArrow _ _) = errorsEM ["Cannot translate function type ", show t]
translateMastType (MastTyCon name tas) = translateTCon name tas

translateTCon :: Name -> [MastTy] -> EM Hull.Type
-- NB "pair" is used for all tuples
translateTCon (Name "pair") tas = translateProductType tas
translateTCon tycon tas = do
  mti <- gets (Map.lookup tycon . ecDT)
  case mti of
    Just (DataTy _n tvs cs) -> do
      let subst = zip tvs (map mastToTy tas)
      tys <- mapM (translateDCon subst) cs
      Hull.TNamed (show tycon) <$> buildSumType tys
    Nothing -> errorsEM ["translateTCon: unknown type ", pretty tycon, "\n", show tycon]
  where
    buildSumType :: [Hull.Type] -> EM Hull.Type
    buildSumType [] = errorsEM ["empty sum ", pretty tycon]
    buildSumType ts = pure (foldr1 Hull.TSum ts)

translateDCon :: [(Tyvar, Ty)] -> Constr -> EM Hull.Type
translateDCon subst (Constr _name tas) = translateProductType (map tyToMast (insts subst tas))

translateProductType :: [MastTy] -> EM Hull.Type
translateProductType [] = pure Hull.TUnit
translateProductType ts = foldr1 Hull.TPair <$> mapM translateMastType ts

emitLit :: Literal -> Hull.Expr
emitLit (IntLit i) = Hull.EWord i
emitLit (StrLit _) = error "String literals not supported yet"

emitConApp :: MastId -> [MastExp] -> Translation Hull.Expr
emitConApp (MastId n ty) as =
  case targetType ty of
    (MastTyCon "unit" []) -> pure (Hull.EUnit, [])
    (MastTyCon "()" []) -> pure (Hull.EUnit, [])
    (MastTyCon "pair" _) -> translateProduct as
    (MastTyCon tcname tas) -> do
      mti <- gets (Map.lookup tcname . ecDT)
      case mti of
        Just (DataTy _ _tvs allCons) -> do
          (prod, code) <- translateProduct as
          hullTargetType <- translateTCon tcname tas
          let result = encodeCon n allCons hullTargetType prod
          pure (result, code)
        Nothing ->
          errors
            [ "emitConApp: unknown type ",
              pretty tcname,
              "\n",
              show tcname
            ]
  where
    targetType :: MastTy -> MastTy
    targetType (MastArrow _ v) = targetType v
    targetType t = t

translateProduct :: [MastExp] -> Translation Hull.Expr
translateProduct [] = pure (Hull.EUnit, [])
translateProduct es = do
  (hullExps, codes) <- unzip <$> mapM emitExp es
  let product = foldr1 (Hull.EPair) hullExps
  pure (product, concat codes)

encodeCon :: Name -> [Constr] -> Hull.Type -> Hull.Expr -> Hull.Expr
encodeCon n [c] _ e | constrName c == n = e
encodeCon n cs (Hull.TNamed l t) e = label l (encodeCon n cs t e)
  where
    label n1 (Hull.EInl t1 e1) = Hull.EInl (Hull.TNamed n1 t1) e1
    label n1 (Hull.EInr t1 e1) = Hull.EInr (Hull.TNamed n1 t1) e1
    label _ expr = expr
encodeCon n (con : cons) t@(Hull.TSum _ t2) e
  | constrName con == n = Hull.EInl t e
  | otherwise = Hull.EInr t (encodeCon n cons t2 e)
encodeCon _ _ t _ =
  errors
    [ "encodeCon: no match for ",
      pretty t,
      "\n",
      show t
    ]

-----------------------------------------------------------------------
-- Translating expressions and statements
-----------------------------------------------------------------------

emitExp :: MastExp -> Translation Hull.Expr
emitExp (MastLit l) = pure (emitLit l, [])
emitExp (MastVar x) = do
  subst <- gets ecSubst
  case Map.lookup (mastIdName x) subst of
    Just e -> pure (e, [])
    Nothing -> pure (Hull.EVar (show (mastIdName x)), [])
-- special handling of revert
emitExp (MastCall (MastId "revert" _) [MastLit (StrLit s)]) = pure (Hull.EUnit, [Hull.SRevert s])
emitExp (MastCall f as) = do
  (hullArgs, codes) <- unzip <$> mapM emitExp as
  let call = Hull.ECall (show (mastIdName f)) hullArgs
  pure (call, concat codes)
emitExp (MastCon i as) = emitConApp i as
emitExp (MastCond e1 e2 e3) = do
  let ty = typeOfMastExp e3
  hullTy <- translateMastType ty
  (ce1, code1) <- emitExp e1
  (ce2, code2) <- emitExp e2
  (ce3, code3) <- emitExp e3
  pure (Hull.ECond hullTy ce1 ce2 ce3, code1 <> code2 <> code3)

emitStmt :: MastStmt -> EM [Hull.Stmt]
emitStmt (MastStmtExp e) = do
  (e', stmts) <- emitExp e
  pure (stmts ++ [Hull.SExpr e'])
emitStmt (MastReturn e) = do
  (e', stmts) <- emitExp e
  let result = stmts ++ [Hull.SReturn e']
  return result
emitStmt (MastAssign i e) = do
  (e', stmts) <- emitExp e
  let assign = [Hull.SAssign (Hull.EVar (show (mastIdName i))) e']
  return (stmts ++ assign)
emitStmt (MastLet (MastId name ty) _mty mexp) = do
  let hullName = show name
  hullTy <- translateMastType ty
  let alloc = [Hull.SAlloc hullName hullTy]
  case mexp of
    Just e -> do
      (v, estmts) <- emitExp e
      let assign = [Hull.SAssign (Hull.EVar hullName) v]
      return (estmts ++ alloc ++ assign)
    Nothing -> return alloc
emitStmt (MastMatch scrutinee alts) = emitMatch scrutinee alts
emitStmt (MastAsm as) = do
  subst <- gets ecSubst
  as' <- renameYulStmts subst as
  pure [Hull.SAssembly as']

emitStmts :: [MastStmt] -> EM [Hull.Stmt]
emitStmts = concatMapM emitStmt'
  where
    emitStmt' stmt = withContext (pretty stmt) (emitStmt stmt)

-----------------------------------------------------------------------
-- Pattern matching
-----------------------------------------------------------------------

{-
After  pattern matching is desugared, we have only simple match statements:
- only one pattern at a time
- no nested patterns

General approach to match statement translation:
1. find scrutinee type
2. fetch constructor list
3. Check for catch-all case alt (opt)
4. Build alternative map, initially containing error or catch-all
5. Translate case alts and insert them into the alt map
6. Build nested match statement from the map

-}

emitMatch :: MastExp -> [MastAlt] -> EM [Hull.Stmt]
emitMatch scrutinee alts = do
  let sty = typeOfMastExp scrutinee
  debug
    [ "emitMatch: ",
      pretty scrutinee,
      " :: ",
      pretty sty,
      "\n",
      unlines $ map (pretty . fst) alts
    ]
  let MastTyCon scon _ = sty
  case scon of
    "word" -> emitWordMatch scrutinee alts
    _ -> emitDataMatch scon scrutinee alts

emitDataMatch :: Name -> MastExp -> [MastAlt] -> EM [Hull.Stmt]
emitDataMatch (Name "pair") scrutinee alts = emitProdMatch scrutinee alts
emitDataMatch (Name "()") scrutinee alts = emitProdMatch scrutinee alts
emitDataMatch scon scrutinee alts = do
  mti <- gets (Map.lookup scon . ecDT)
  let ti = fromMaybe (errors ["emitMatch: unknown type " ++ show scon]) mti
  let allCons = dataConstrs ti
  case allCons of
    [] -> errorsEM ["emitMatch: no constructors for ", pretty scon]
    [_] -> emitProdMatch scrutinee alts
    _ -> emitSumMatch allCons scrutinee alts

emitWordMatch :: MastExp -> [MastAlt] -> EM [Hull.Stmt]
emitWordMatch scrutinee alts = do
  (sVal, sCode) <- emitExp scrutinee
  let hullType = Hull.TWord
  hullAlts <- mapM (emitWordAlt sVal) alts
  return (sCode ++ [Hull.SMatch hullType sVal hullAlts])
  where
    emitWordAlt :: Hull.Expr -> MastAlt -> EM Hull.Alt
    emitWordAlt _ (MastPLit (IntLit i), stmts) = Hull.Alt (Hull.PIntLit i) "$_" <$> emitStmts stmts
    emitWordAlt sVal (MastPVar (MastId n _), stmts) = withLocalState do
      extendVSubst (Map.singleton n sVal)
      hullStmts <- emitStmts stmts
      let hullName = show n
      return (Hull.Alt (Hull.PVar hullName) "$_" hullStmts)
    emitWordAlt _ (pat, _) = errorsEM ["emitWordAlt not implemented for", show pat]

-- prebranch reflects the fact that we don't know its position (Inl/Inr/InK) yet, just a name and body
type HullPreBranch = (Hull.Name, [Hull.Stmt])

type PreBranchMap = Map.Map Name HullPreBranch

emitSumMatch :: [Constr] -> MastExp -> [MastAlt] -> EM [Hull.Stmt]
emitSumMatch allCons scrutinee alts = do
  (sVal, sCode) <- emitExp scrutinee
  let sType = typeOfMastExp scrutinee
  sHullType <- translateMastType sType
  let noMatch c = ("_", [Hull.SRevert ("no match for: " ++ show c)])
  debug ["emitMatch: allCons ", show allConNames]
  let defaultBranchMap = Map.fromList [(c, noMatch c) | c <- allConNames]
  branches <- emitEqns alts
  let branchMap = foldr insertBranch defaultBranchMap branches
  let orderedBranches = [branchMap Map.! c | c <- allConNames]
  debug ["emitMatch: branches ", show orderedBranches]
  matchCode <- buildMatch sVal sHullType orderedBranches
  return (sCode ++ matchCode)
  where
    allConNames = map constrName allCons
    insertBranch :: (MastPat, HullPreBranch) -> PreBranchMap -> PreBranchMap
    insertBranch (MastPVar _, stmts) _ = Map.fromList [(c, stmts) | c <- allConNames]
    insertBranch (MastPCon (MastId n _) _, stmts) m = Map.insert n stmts m
    insertBranch _ _ = error "emitSumMatch.insertBranch: unexpected pattern"

    emitEqn :: MastAlt -> EM (MastPat, HullPreBranch)
    emitEqn (pat, stmts) = withLocalState do
      aname <- freshAltName
      let expr = Hull.EVar aname
      let pvars = case pat of
            MastPVar (MastId n _) -> Map.singleton n expr
            MastPCon _ patargs -> translateMastPatArgs expr patargs
            _ -> Map.empty
      extendVSubst pvars
      let comment = Hull.SComment (pretty pat)
      hullStmts <- emitStmts stmts
      let hullStmts' = comment : hullStmts
      debug ["emitEqn: ", show pat, " / ", show expr, " -> ", show hullStmts']
      return (pat, (aname, hullStmts'))

    emitEqns :: [MastAlt] -> EM [(MastPat, HullPreBranch)]
    emitEqns = mapM emitEqn

    -- buildMatch takes a list of branches for a sum type and builds a nested match statement
    -- arguments:
    -- sval: the scrutinee value
    -- sty: the scrutinee type
    -- branches: a list of branches, where each branch is a list of statements
    buildMatch :: Hull.Expr -> Hull.Type -> [HullPreBranch] -> EM [Hull.Stmt]
    buildMatch _sval _sty [] = error "buildMatch: empty branch list"
    buildMatch sval0 sty branches = go sval0 sty branches
      where
        go :: Hull.Expr -> Hull.Type -> [HullPreBranch] -> EM [Hull.Stmt]
        go _sval _sty [(_, b)] = pure b
        go sval curTy ((leftVar, b) : bs@((rightVar, _) : _)) = do
          rest <- go (Hull.EVar rightVar) (rightBranch curTy) bs
          pure
            [ Hull.SMatch
                curTy
                sval
                [ alt Hull.CInl leftVar b,
                  alt Hull.CInr rightVar rest
                ]
            ]
        go _ _ [] = error "buildMatch/go - impossible: empty branch list"
        rightBranch (Hull.TSum _ r) = r
        rightBranch (Hull.TNamed _ t) = rightBranch t
        rightBranch t = error ("rightBranch: not a sum type: " ++ show t)

        alt con n stmts = Hull.ConAlt con n stmts

freshAltName :: EM String
freshAltName = do
  i <- freshInt
  return ("$alt" ++ show i)

emitProdMatch :: MastExp -> [MastAlt] -> EM [Hull.Stmt]
emitProdMatch scrutinee (eqn : _) = do
  (sexpr, scode) <- emitExp scrutinee
  mcode <- translateSingleMastAlt sexpr eqn
  return (scode ++ mcode)
emitProdMatch _ [] = errorsEM ["emitProdMatch: no alternatives"]

-- | translateSingleMastAlt handles the special case for product types
-- there is only one match branch, just transform to projections
translateSingleMastAlt :: Hull.Expr -> MastAlt -> EM [Hull.Stmt]
translateSingleMastAlt expr (MastPCon _con patargs, stmts) = withLocalState do
  let pvars = translateMastPatArgs expr patargs
  debug ["! translateSingleMastAlt: pvars = ", show pvars]
  extendVSubst pvars
  emitStmts stmts
translateSingleMastAlt _ (pat, _) =
  error $
    "translateSingleMastAlt: expected constructor pattern, got: " ++ show pat

-- translate pattern arguments to a substitution, e.g.
-- p@(Just x) ~> [x -> p]
-- p@(Pair x y) ~> [x -> fst p, y -> snd p]
-- p@(Triple x y z) ~> [x -> fst p, y -> fst (snd p), z -> snd (snd p)]
translateMastPatArgs :: Hull.Expr -> [MastPat] -> VSubst
translateMastPatArgs e = Map.fromList . go e
  where
    go _ [] = []
    go s [MastPVar i] = [(mastIdName i, s)]
    go s (MastPVar i : as) =
      let (s1, s2) = (Hull.EFst s, Hull.ESnd s)
       in (mastIdName i, s1) : go s2 as
    go s (MastPCon _ [] : as) = go s as
    go _ (pat : _) = error ("Unimplemented: translateMastPatArgs _ " ++ show pat)

-----------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------

debug :: [String] -> EM ()
debug msg = do
  enabled <- gets ecDebug
  when enabled $ writes msg

concatMapM :: (Monad f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f xs = concat <$> mapM f xs

-----------------------------------------------------------------------
-- Yul AST Renaming
-----------------------------------------------------------------------

-- | Renames variables in a Yul AST based on a substitution map.
-- It correctly handles lexical scoping of Yul statements.
-- Free variables matching the VSubst map are replaced.
-- If they map to a simple EVar, they are substituted inline.
-- If they map to complex expressions, an error is thrown.
renameYulStmts :: VSubst -> [YulStmt] -> EM [YulStmt]
renameYulStmts subst stmts = goBlock Set.empty stmts
  where
    goBlock :: Set.Set Name -> [YulStmt] -> EM [YulStmt]
    goBlock _ [] = pure []
    goBlock scope (s : ss) = do
      (s', scope') <- goStmt scope s
      ss' <- goBlock scope' ss
      pure (s' : ss')

    goStmt :: Set.Set Name -> YulStmt -> EM (YulStmt, Set.Set Name)
    goStmt scope (YBlock blk) = do
      blk' <- goBlock scope blk
      pure (YBlock blk', scope)
    goStmt scope (YFun f args rets blk) = do
      let scope' = scope `Set.union` Set.fromList (args ++ fromMaybe [] rets)
      blk' <- goBlock scope' blk
      pure (YFun f args rets blk', Set.insert f scope)
    goStmt scope (YLet names mexp) = do
      mexp' <- mapM (goExp scope) mexp
      let scope' = scope `Set.union` Set.fromList names
      pure (YLet names mexp', scope')
    goStmt scope (YAssign names expr) = do
      expr' <- goExp scope expr
      names' <- mapM (renameName scope) names
      pure (YAssign names' expr', scope)
    goStmt scope (YIf cond blk) = do
      cond' <- goExp scope cond
      blk' <- goBlock scope blk
      pure (YIf cond' blk', scope)
    goStmt scope (YSwitch cond cases def) = do
      cond' <- goExp scope cond
      cases' <- mapM (\(l, b) -> (l,) <$> goBlock scope b) cases
      def' <- mapM (goBlock scope) def
      pure (YSwitch cond' cases' def', scope)
    goStmt scope (YFor pre cond post blk) = do
      let preScope = scope `Set.union` getDecls pre
      pre' <- goBlock preScope pre
      cond' <- goExp preScope cond
      post' <- goBlock preScope post
      blk' <- goBlock preScope blk
      pure (YFor pre' cond' post' blk', scope)
    goStmt scope s@(YBreak) = pure (s, scope)
    goStmt scope s@(YContinue) = pure (s, scope)
    goStmt scope s@(YLeave) = pure (s, scope)
    goStmt scope s@(YComment _) = pure (s, scope)
    goStmt scope (YExp e) = do
      e' <- goExp scope e
      pure (YExp e', scope)

    goExp :: Set.Set Name -> YulExp -> EM YulExp
    goExp scope (YIdent n) = YIdent <$> renameName scope n
    goExp scope (YCall f args) = do
      f' <- renameName scope f
      args' <- mapM (goExp scope) args
      pure (YCall f' args')
    goExp _ e = pure e

    renameName :: Set.Set Name -> Name -> EM Name
    renameName scope n
      | not (Set.member n scope),
        Just e <- Map.lookup n subst = do
          case e of
            Hull.EVar newName -> pure (fromString newName)
            -- FUTURE WORK (Materializing complex expressions):
            -- If `e` is a complex expression like `EFst p`, it cannot be substituted inline in Yul.
            -- To fix this properly later:
            -- 1. Instead of throwing an error, record `n -> e` in a State map.
            -- 2. Leave `YIdent n` unchanged inside the block.
            -- 3. In `emitStmt (MastAsm as)`, emit `SAlloc n TWord` and `SAssign n e` BEFORE the block.
            -- 4. Remove `n` from `ecSubst` so subsequent reads outside the block see the mutated Yul variable.
            _ -> errorsEM ["not implemented: Yul assembly block references variable '", show n, "' mapped to complex expression '", show e, "'"]
      | otherwise = pure n

    getDecls :: [YulStmt] -> Set.Set Name
    getDecls [] = Set.empty
    getDecls (YLet names _ : ss) = Set.fromList names `Set.union` getDecls ss
    getDecls (YFun f _ _ _ : ss) = Set.insert f (getDecls ss)
    getDecls (_ : ss) = getDecls ss
