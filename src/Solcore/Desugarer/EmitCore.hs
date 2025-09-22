module Solcore.Desugarer.EmitCore(emitCore) where
import Prelude hiding(catch, product)
import Language.Core qualified as Core
import Data.Map qualified as Map
import Common.Monad
import Control.Monad(when, unless)
import Control.Monad.Reader.Class ()
import Control.Monad.State
import Data.Maybe(fromMaybe)
import GHC.Stack( HasCallStack )

import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Frontend.TypeInference.TcEnv(TcEnv(..),TypeInfo(..), TypeTable)
import Solcore.Frontend.TypeInference.TcMonad (insts)
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives
import Solcore.Desugarer.Specialise(typeOfTcExp)
import System.Exit

emitCore :: Bool -> TcEnv ->  CompUnit Id -> IO [Core.Contract]
emitCore debugp env cu = fmap concat $ runEM debugp env $ mapM emitTopDecl (contracts cu)

type EM a = StateT EcState IO a
runEM :: Bool -> TcEnv -> EM a ->  IO a
runEM debugp env m = evalStateT m (initEcState debugp env)

errorsEM :: HasCallStack => [String] -> EM a
errorsEM msgs = do
  writes ("\nERROR: ":msgs)
  context <- gets ecContext
  let msg = concat msgs
  let contextStr = unlines (map ("in: "++) context)
  writeln contextStr
  error "Emit core failed" -- this can be exitFailure eventually

data EcState = EcState
    { ecSubst :: VSubst
    , ecDT :: DataTable
    , ecNest :: Int
    , ecDebug :: Bool
    , ecContext :: [String] -- for error handling
    }

initEcState :: Bool -> TcEnv -> EcState
initEcState debugp env = EcState
   { ecSubst = emptyVSubst
   , ecDT = Map.fromList builtinDataInfo
   , ecNest = 0
   , ecDebug = debugp
   , ecContext = []
   }

withLocalState :: EM a -> EM a
withLocalState m = do
    s <- get
    a <- m
    put s
    return a

type DataTable = Map.Map Name DataTy
-- type DataTable = Map.Map Name TConInfo
type TConInfo = ([Tyvar], [DConInfo]) -- type con: type vars and data cons
type DConInfo = (Name, [Ty])          -- data con: name and argument types

sumDataTy :: DataTy
sumDataTy = DataTy
  { dataName = "sum"
  , dataParams = [TVar "a", TVar "b"]
  , dataConstrs = [ Constr "inl" [tyvar "a"]
                  , Constr "inr" [tyvar "b"]
                  ]
  } where
    tyvar = TyVar . TVar

builtinDataInfo = [ ("sum", sumDataTy) ]

type VSubst = Map.Map Name Core.Expr
emptyVSubst :: VSubst
emptyVSubst = Map.empty

extendVSubst :: VSubst -> EM ()
extendVSubst subst = modify extend where
    extend s = s { ecSubst = ecSubst s <> subst }

pushContext :: String -> EM ()
pushContext c = modify extend where
    extend s = s { ecContext = c : ecContext s }

dropContext :: EM ()
dropContext = modify (\s -> s { ecContext = drop 1 $ ecContext s })

withContext :: String -> EM a -> EM a
withContext s m = pushContext s *> m <* dropContext

inContext :: EM a -> String -> EM a
inContext = flip withContext

type Translation a = EM (a, [Core.Stmt])

type CoreName = String

emitTopDecl :: TopDecl Id -> EM [Core.Contract]
emitTopDecl (TContr c) = fmap pure (emitContract c)
emitTopDecl (TDataDef dt) = addData dt >> pure []
emitTopDecl _ = pure []

addData :: DataTy -> EM ()
addData dt = modify (\s -> s { ecDT = Map.insert (dataName dt) dt (ecDT s) })

{-
buildTConInfo :: DataTy -> TConInfo
buildTConInfo (DataTy n tvs dcs) = (tvs, map conInfo dcs) where
  conInfo (Constr n ts) = (n, ts)
-}
emitContract :: Contract Id -> EM Core.Contract
emitContract c = do
    let cname = show (name c)
    writes ["Emitting core for contract ", cname]
    coreBody <- concatMapM emitCDecl (decls c)
    let result = Core.Contract cname coreBody
    -- writeln (show result)
    pure result

emitCDecl :: ContractDecl Id -> EM [Core.Stmt]
emitCDecl cd@(CFunDecl f) = do
    -- debug ["!! emitCDecl ", show cd]
    emitFunDef f
emitCDecl cd@(CDataDecl dt) = do
    -- debug ["!! emitCDecl ", show cd]
    addData dt >> pure []
emitCDecl cd = debug ["!! emitCDecl ", show cd] >> pure []

-----------------------------------------------------------------------
-- Translating function definitions
-----------------------------------------------------------------------
emitFunDef :: HasCallStack => FunDef Id -> EM [Core.Stmt]
emitFunDef fd@(FunDef sig body) = withContext (shortName fd) do
  (name, args, typ) <- translateSig sig `inContext` ("function signature " ++ pretty sig)
  debug ["\n# emitFunDef ", name, " :: ", show typ]
  coreBody <- emitStmts body
  let coreFun = Core.SFunction name args typ coreBody
  dropContext
  return [coreFun]

translateSig :: HasCallStack => Signature Id -> EM (CoreName, [Core.Arg], Core.Type)
translateSig sig@(Signature vs ctxt n args (Just ret)) = do
  dataTable <- gets ecDT
  -- debug ["translateSig ", show sig]
  let name = show n
  coreTyp <- translateType ret
  coreArgs <- mapM translateArg args
  return (name, coreArgs, coreTyp)
translateSig sig = errorsEM ["No return type in ", show sig]

translateArg :: Param Id -> EM Core.Arg
translateArg p =  Core.TArg (show n) <$> translateType t
    where Id n t = getParamId p

getParamId :: Param Id -> Id
getParamId (Typed i _) = i
getParamId (Untyped i) = i

-----------------------------------------------------------------------
-- Translating types and value constructors
-----------------------------------------------------------------------

translateType :: HasCallStack => Ty -> EM Core.Type
translateType (TyCon "word" []) = pure Core.TWord
-- translateType _ Fun.TBool = Core.TBool
translateType (TyCon "unit" []) = pure Core.TUnit
translateType (TyCon "()" []) = pure Core.TUnit
translateType t@(u :-> v) = errorsEM ["Cannot translate function type ", show t]
translateType (TyCon name tas) = translateTCon name tas
translateType t = errorsEM ["Cannot translate type ", show t]

translateTCon :: Name -> [Ty] -> EM Core.Type
-- NB "pair" is used for all tuples
translateTCon (Name "pair") tas = translateProductType tas
translateTCon tycon tas = do
    mti <- gets (Map.lookup tycon . ecDT)
    case mti of
        Just (DataTy n tvs cs) -> do
            let subst = zip tvs tas
            tys <- mapM (translateDCon subst) cs
            Core.TNamed (show tycon) <$> buildSumType tys
        Nothing -> errorsEM ["translateTCon: unknown type ", pretty tycon, "\n", show tycon]
  where
      buildSumType :: [Core.Type] -> EM Core.Type
      buildSumType [] = errorsEM ["empty sum ", pretty tycon] -- Core.TUnit
      buildSumType ts = pure(foldr1 Core.TSum ts)


translateDCon :: [(Tyvar, Ty)] -> Constr -> EM Core.Type
translateDCon subst  (Constr name tas) = translateProductType (insts subst tas)

translateProductType :: [Ty] -> EM Core.Type
translateProductType [] = pure Core.TUnit
translateProductType ts = foldr1 Core.TPair <$> mapM translateType ts

emitLit :: Literal -> Core.Expr
emitLit (IntLit i) = Core.EWord i
emitLit (StrLit s) = error "String literals not supported yet"

emitConApp :: Id -> [Exp Id] -> Translation Core.Expr
emitConApp con@(Id n ty) as = do
  unless (null . fv $ argTypes ty)
    (errors ["emitConApp: free variables in type ", pretty ty, " in ", pretty (Con con as)])
  -- check for free type vars only in args because of phantom types such as Proxy(a) = Proxy
  case targetType ty of
    (TyCon "unit" []) -> pure (Core.EUnit, [])
    (TyCon "()" []) -> pure (Core.EUnit, [])
    (TyCon "pair" _) -> translateProduct as
    (TyCon tcname tas) -> do
        mti <- gets (Map.lookup tcname . ecDT)
        case mti of
            Just (DataTy _ tvs allCons) -> do
                (prod, code) <- translateProduct as
                coreTargetType <- translateTCon tcname tas
                let result = encodeCon n allCons coreTargetType prod
                pure (result, code)
            Nothing -> errors
                [ "emitConApp: unknown type ", pretty tcname
                , "\n", show tcname
                , "\n", "In:", pretty baddie, "\n", show baddie
                ] where baddie = Con con as
    otherType -> errors
        [ "emitConApp: not a type constructor: ", pretty otherType
        , "\n", show otherType
        ]
  where
    targetType :: Ty -> Ty
    targetType (u :-> v) = targetType v
    targetType t = t
    argTypes :: Ty -> [Ty]
    argTypes (u :-> v) = u : argTypes v
    argTypes t = []

translateProduct :: [Exp Id] -> Translation Core.Expr
translateProduct [] = pure (Core.EUnit, [])
translateProduct es = do
    (coreExps, codes) <- unzip <$> mapM emitExp es
    let product = foldr1 (Core.EPair) coreExps
    pure (product, concat codes)

encodeCon :: Name ->[Constr] -> Core.Type -> Core.Expr -> Core.Expr
encodeCon n [c] _ e | constrName c == n = e
encodeCon n cs (Core.TNamed l t) e = label l (encodeCon n cs t e)  -- this will change when we compress tags
  where label l (Core.EInl t e) = Core.EInl (Core.TNamed l t) e
        label l (Core.EInr t e) = Core.EInr (Core.TNamed l t) e
        label l e = e
encodeCon n (con:cons) t@(Core.TSum t1 t2) e
    | constrName con == n = Core.EInl t e
    | otherwise = Core.EInr t (encodeCon n cons t2 e)
encodeCon n cons t e = errors
    [ "encodeCon: no match for ", pretty t
    , "\n", show  t
    ]
-----------------------------------------------------------------------
-- Translating expressions and statements
-----------------------------------------------------------------------

emitExp :: Exp Id -> Translation Core.Expr
emitExp (Lit l) = pure (emitLit l, [])
emitExp (Var x) = do
    subst <- gets ecSubst
    case Map.lookup (idName x) subst of
        Just e -> pure (e, [])
        Nothing -> pure (Core.EVar (unwrapId x), [])
-- special handling of revert
emitExp (Call _ (Id "revert" _) [Lit(StrLit s)]) = pure(Core.EUnit, [Core.SRevert s])
emitExp (Call Nothing f as) = do
    (coreArgs, codes) <- unzip <$> mapM emitExp as
    let call =  Core.ECall (unwrapId f) coreArgs
    pure (call, concat codes)
emitExp e@(Con i as) = emitConApp i as
emitExp (TyExp e _) = emitExp e

emitExp (Cond e1 e2 e3) = do
  let ty = typeOfTcExp e3
  coreTy <- translateType ty
  (ce1, code1) <- emitExp e1
  (ce2, code2) <- emitExp e2
  (ce3, code3) <- emitExp e3
  pure (Core.ECond coreTy ce1 ce2 ce3, code1 <> code2 <> code3)

emitExp e = errorsEM ["emitExp not implemented for: ", pretty e, "\n", show e]

emitStmt :: Stmt Id -> EM [Core.Stmt]
emitStmt (StmtExp e) = do
    (e', stmts) <- emitExp e
    pure (stmts ++ [Core.SExpr e'])
emitStmt s@(Return e) = do
    (e', stmts) <- emitExp e
    let result = stmts ++ [Core.SReturn e']
    --- debug ["<  emitStmt ", show (Core.Core result)]
    return result

emitStmt (Var i := e) = do
    (e', stmts) <- emitExp e
    let assign = [Core.SAssign (Core.EVar (unwrapId i)) e']
    return (stmts ++ assign)

emitStmt (Let (Id name ty) mty mexp ) = do
    let coreName = show name
    coreTy <- translateType ty
    let alloc = [Core.SAlloc coreName coreTy]
    case mexp of
        Just e -> do
            (v, estmts) <- emitExp e
            let assign = [Core.SAssign (Core.EVar coreName) v]
            return (estmts ++ alloc ++ assign)
        Nothing -> return alloc

emitStmt s@(Match [scrutinee] alts) = emitMatch scrutinee alts
emitStmt (Asm ys) = pure [Core.SAssembly ys]
emitStmt s = errorsEM ["emitStmt not implemented for: ", pretty s, "\n", show s]

emitStmts :: [Stmt Id] -> EM [Core.Stmt]
emitStmts = concatMapM emitStmt' where
    emitStmt' stmt =  withContext (pretty stmt) (emitStmt stmt)

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



emitMatch :: Exp Id -> Equations Id -> EM [Core.Stmt]
emitMatch scrutinee alts = do
    let sty =  typeOfTcExp scrutinee

    debug [ "emitMatch: ", pretty scrutinee, " :: ", pretty sty
          , "\n", unlines $ map pretty alts]
    let scon = case sty of
            TyCon n _ -> n
            _ -> error ("emitMatch: scrutinee not a type constructor: " ++ show sty)
    case scon of
        "word" -> emitWordMatch scrutinee alts
        _ -> emitDataMatch scon scrutinee alts

emitDataMatch :: Name -> Exp Id -> Equations Id -> StateT EcState IO [Core.Stmt]
emitDataMatch (Name "pair") scrutinee alts = emitProdMatch scrutinee alts
emitDataMatch (Name "()" ) scrutinee alts = emitProdMatch scrutinee alts
emitDataMatch scon scrutinee alts = do
    mti <- gets (Map.lookup scon . ecDT)
    let ti = fromMaybe (errors ["emitMatch: unknown type " ++ show scon]) mti
    let allCons = dataConstrs ti
    case allCons of
        [] -> errorsEM ["emitMatch: no constructors for ", pretty scon]
        [c] -> emitProdMatch scrutinee alts
        _ -> emitSumMatch allCons scrutinee alts

emitWordMatch :: Exp Id -> Equations Id -> EM [Core.Stmt]
emitWordMatch scrutinee alts = do
    (sVal, sCode) <- emitExp scrutinee
    let coreType = Core.TWord
    coreAlts <- mapM emitWordAlt alts
    return [Core.SMatch coreType sVal coreAlts]
    where
        emitWordAlt :: Equation Id -> EM Core.Alt
        emitWordAlt ([PLit(IntLit i)], stmts) = Core.Alt (Core.PIntLit i) "$_" <$> emitStmts stmts
        emitWordAlt ([PVar (Id n _)], stmts) = do
            coreStmts <- emitStmts stmts
            let coreName = show n
            return (Core.Alt (Core.PVar coreName) "$_" coreStmts)
        emitWordAlt (pat, _) = errorsEM ["emitWordAlt not implemented for", show pat]

type BranchMap = Map.Map Name [Core.Stmt]

emitSumMatch :: [Constr] -> Exp Id -> Equations Id -> EM [Core.Stmt]
emitSumMatch allCons scrutinee alts = do
    (sVal, sCode) <- emitExp scrutinee
    let sType = typeOfTcExp scrutinee
    sCoreType <- translateType sType
    -- TODO: build branch list in order matching allCons
    -- by inserting them into a map and then outputting in order
    -- take default branch from last equation into account
    let noMatch c = [Core.SRevert ("no match for: "++ show c)]
    debug ["emitMatch: allCons ", show allConNames]
    let defaultBranchMap = Map.fromList [(c, noMatch c) | c <- allConNames]
    branches <- emitEqns alts
    let branchMap = foldr insertBranch defaultBranchMap branches
    let branches = [branchMap Map.! c | c <- allConNames]
    debug ["emitMatch: branches ", show branches]
    let matchCode = buildMatch sVal sCoreType branches
    return(sCode ++ matchCode)
    where
      allConNames = map constrName allCons
      insertBranch :: (Pat Id, [Core.Stmt]) -> BranchMap -> BranchMap
      insertBranch (PVar (Id n _), stmts) m = Map.fromList [(c, stmts) | c <- allConNames]
      insertBranch (PCon (Id n _) _, stmts) m = Map.insert n stmts m
      emitEqn :: Core.Expr -> Equation Id -> EM (Pat Id, [Core.Stmt])
      emitEqn expr ([pat], stmts) = withLocalState do
        let patargs = getPatArgs pat
        let pvars = translatePatArgs expr patargs
        extendVSubst pvars
        let comment = Core.SComment (pretty pat)
        coreStmts <- emitStmts stmts
        let coreStmts' = comment : coreStmts
        debug ["emitEqn: ", show pat, " / ", show expr, " -> ", show coreStmts']
        return (pat, coreStmts')
      emitEqn _ _ = error ("emitEqn: multiple patterns should have been desugared by now")
      getPatArgs(PCon _ patargs) = patargs
      getPatArgs _ = []

      -- TODO: emitEqns should process the eqns in constructor declaration order
      -- e.g. if we have data B = F | T and then match b | T => ... | F => ...
      -- we should still process the F case first to avoid mixing up inl/inr
      emitEqns :: [Equation Id] -> EM [(Pat Id, [Core.Stmt])]
      emitEqns [eqn] = (:[]) <$> emitEqn (Core.EVar (altName True)) eqn
      emitEqns (eqn:eqns) = do
        b <- emitEqn (Core.EVar (altName False)) eqn
        bs <- emitEqns eqns
        return (b:bs)

      {- buildMatch builds a nested match statement from a list of branches
         e.g. [b1, b2, b3] yields
            match<type> s with {
                inl $left => b1
                inr $right => match $right with {
                    inl $left => b2
                    inr $right => b3
                }
            }

          The names $left and $right are used for clarity,
          they can be reused in subsequent branches (no need for unique names
          as long as they do not clash with user variables)
      -}
      buildMatch :: Core.Expr -> Core.Type ->[[Core.Stmt]] -> [Core.Stmt]
      buildMatch _sval sty [] = error "buildMatch: empty branch list"
      buildMatch sval0 sty branches = go sval0 sty branches where
        go _sval sty  [b] = b -- last branch needs no match
        go sval sty (b:bs) =  [Core.SMatch sty sval [ alt Core.CInl left b
                          , alt Core.CInr right (go (Core.EVar right) (rightBranch sty) bs)]]
        rightBranch (Core.TSum _ r) = r
        rightBranch (Core.TNamed _ t) = rightBranch t
        rightBranch t = error ("rightBranch: not a sum type: " ++ show t)
        left = altName False
        right = altName True
        alt con n stmts = Core.ConAlt con n stmts

      -- Would be clearer with $left/$right, but simpler with $alt for now
      altName False = "$alt"
      altName True = "$alt"

emitProdMatch :: Exp Id -> Equations Id -> EM [Core.Stmt]
emitProdMatch scrutinee (eqn:_) = do
    (sexpr, scode) <- emitExp scrutinee
    mcode <- translateSingleEquation sexpr eqn
    return (scode ++ mcode)


-- | translateSingleEquation handles the special case for product types
-- there is only one match branch, just transform to projections
-- takes a translated scrutinee and a single equation
translateSingleEquation :: Core.Expr -> Equation Id -> EM [Core.Stmt]
translateSingleEquation expr ([PCon _con patargs], stmts) = withLocalState do
    let pvars = translatePatArgs expr patargs
    extendVSubst pvars
    emitStmts stmts
translateSingleEquation _ eqn  = error $
  "translateSingleEquation: there should be exactly one pattern\n" ++ pretty eqn

-- translate pattern arguments to a substitution, e.g.
-- p@(Just x) ~> [x -> p]
-- p@(Pair x y) ~> [x -> fst p, y -> snd p]
-- p@(Triple x y z) ~> [x -> fst p, y -> fst (snd p), z -> snd (snd p)]
translatePatArgs :: Core.Expr -> [Pat Id] -> VSubst
translatePatArgs e = Map.fromList . go e where
    go _ [] = []
    go s [PVar i] = [(idName i, s)]
    go s (PVar i:as) = let (s1, s2) = (Core.EFst s, Core.ESnd s) in
        (idName i, s1) : go s2 as
    go _ (pat:_) = error ("Unimplemented: translatePatArgs _ " ++ pretty pat)

-----------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------

debug :: [String] -> EM ()
debug msg = do
    enabled <- gets ecDebug
    when enabled $ writes msg

{-
dumpDT :: EM ()
dumpDT = do
    tis <- gets (Map.toList . ecDT)
    debug ["Data: ", unlines (map show tis)]
    -- debug ["Data: ", show tis]
-}

concatMapM :: (Monad f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f xs = concat <$> mapM f xs

unwrapId :: Id -> CoreName
unwrapId = show . idName
