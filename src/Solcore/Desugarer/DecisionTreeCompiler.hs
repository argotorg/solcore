module Solcore.Desugarer.DecisionTreeCompiler where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics (everywhere, mkT)
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord (comparing)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Primitives.Primitives

-- main function for pattern compilation

matchCompiler :: CompUnit Id -> IO (Either String (CompUnit Id, [Warning]))
matchCompiler cunit =
  do
    result <- runCompilerM (initialTypeEnv cunit) (compile cunit)
    case result of
      Left err -> pure $ Left err
      Right (unit', warns) -> do
        let (nonExaustive, redundant) = partition isNonExaustive warns
        if null nonExaustive
          then
            pure $ Right (unit', redundant)
          else pure $ Left $ unlines $ map showWarning nonExaustive

-- type class for the defining the pattern matching compilation
-- over the syntax tree

class Compile a where
  compile :: a -> CompilerM a

instance (Compile a) => Compile [a] where
  compile = mapM compile

instance (Compile a) => Compile (Maybe a) where
  compile Nothing = pure Nothing
  compile (Just x) = Just <$> compile x

instance Compile (CompUnit Id) where
  compile (CompUnit imps ds) =
    CompUnit imps <$> compile ds

instance Compile (TopDecl Id) where
  compile (TContr c) =
    TContr <$> compile c
  compile (TFunDef f) =
    TFunDef <$> compile f
  compile (TInstDef d) =
    TInstDef <$> compile d
  compile (TMutualDef ds) =
    TMutualDef <$> compile ds
  compile d = pure d

instance Compile (Contract Id) where
  compile (Contract n vs ds) =
    Contract n vs
      <$> local (\(te, ctx) -> (Map.union env' te, ctx ++ ["contract " ++ pretty n])) (compile ds)
    where
      ds' = [d | (CDataDecl d) <- ds]
      env' = foldr addDataTyInfo Map.empty ds'

instance Compile (ContractDecl Id) where
  compile (CFunDecl d) =
    CFunDecl <$> compile d
  compile (CMutualDecl ds) =
    CMutualDecl <$> compile ds
  compile (CConstrDecl c) =
    CConstrDecl <$> compile c
  compile d = pure d

instance Compile (Constructor Id) where
  compile (Constructor ps bd) =
    Constructor ps <$> compile bd

instance Compile (FunDef Id) where
  compile (FunDef sig bd) =
    FunDef sig <$> pushCtx ("function " ++ pretty (sigName sig)) (compile bd)

instance Compile (Stmt Id) where
  compile (e1 := e2) =
    (:=) <$> compile e1 <*> compile e2
  compile (Let v mt me) =
    Let v mt <$> compile me
  compile (StmtExp e) =
    StmtExp <$> compile e
  compile (Return e) =
    Return <$> compile e
  compile (If e1 bd1 bd2) =
    If <$> compile e1 <*> compile bd1 <*> compile bd2
  compile s@(Asm _) =
    pure s
  compile (Match es eqns) = do
    es' <- compile es
    eqns' <- mapM compileEqn eqns
    scrutTys <- mapM scrutineeType es'
    let occs = [[i] | i <- [0 .. length es' - 1]]
        occMap = Map.fromList (zip occs es')
        matrix = map fst eqns'
        actions = map snd eqns'
        bacts = [([], a) | a <- actions]
        matchDesc = "match (" ++ intercalate ", " (map pretty es') ++ ")"
    tree <- pushCtx matchDesc $ compileMatrix scrutTys occs matrix bacts
    treeToStmt occMap tree

compileEqn :: Equation Id -> CompilerM (Equation Id)
compileEqn (pats, stmts) = (pats,) <$> compile stmts

compileMatrix ::
  [Ty] ->
  [Occurrence] ->
  PatternMatrix ->
  [BoundAction] ->
  CompilerM DecisionTree
compileMatrix tys _ [] _ = do
  pats <- mapM freshPat tys
  ctx <- askWarnCtx
  unless (null pats) $ tell [NonExhaustive ctx pats]
  pure Fail
compileMatrix tys occs (firstRow : restRows) ((firstBinds, firstAct) : _)
  | all isVarPat firstRow = do
      mw <-
        if null tys
          then pure (Just [])
          else inhabitsM restRows tys
      ctx <- askWarnCtx
      let redundant = isNothing mw
          warns = [RedundantClause ctx firstRow firstAct | redundant]
          varBinds = [(v, occ) | (PVar v, occ) <- zip firstRow occs]
      tell warns
      pure (Leaf (firstBinds ++ varBinds) firstAct)
compileMatrix tys occs matrix bacts = do
  let col = selectColumn occs matrix
      matrix' = map (reorderList col) matrix
      occs' = reorderList col occs
      tys' = reorderList col tys
  case (tys', occs', matrix') of
    (_ : restTys, testOcc : restOccs, _) -> do
      let firstCol = [p | (p : _) <- matrix']
          headCons = nub (mapMaybe patHeadCon firstCol)
          headLits = nub (mapMaybe patHeadLit firstCol)
      case (headCons, headLits) of
        (c : cs, _) ->
          buildConSwitch testOcc restOccs restTys matrix' bacts (c : cs)
        ([], ls@(_ : _)) ->
          buildLitSwitch testOcc restOccs restTys matrix' bacts ls
        ([], []) ->
          -- All wildcards after reordering: strip column and continue
          compileMatrix restTys restOccs (defaultMatrix matrix') (defaultBoundActs testOcc matrix' bacts)
    -- Unreachable: tys, occs, and matrix are parallel and all non-empty here
    _ -> pure Fail

buildConSwitch ::
  Occurrence ->
  [Occurrence] ->
  [Ty] ->
  PatternMatrix ->
  [BoundAction] ->
  [Id] ->
  CompilerM DecisionTree
buildConSwitch testOcc restOccs restTys matrix bacts headCons = do
  branches <- mapM buildBranch headCons
  complete <- isComplete headCons
  mDefault <-
    if complete
      then pure Nothing
      else buildDefault
  pure (Switch testOcc branches mDefault)
  where
    buildBranch k = do
      info <- lookupConInfo (idName k)
      let fieldTys = conFieldTypes info
          newOccs = childOccs testOcc (length fieldTys) ++ restOccs
          newTys = fieldTys ++ restTys
          specBacts = specializedBoundActs k testOcc matrix bacts
          rawPats = case [ps | (PCon k' ps : _) <- matrix, idName k' == idName k] of
            (ps : _) -> ps
            [] -> []
      -- Ensure srcPats are all PVar: PCon sub-patterns get a fresh variable
      -- so conBranchToEqn can extend the occMap for child occurrences.
      srcPats <- mapM ensureVar rawPats
      specMat <- specialize k fieldTys matrix
      subTree <- compileMatrix newTys newOccs specMat specBacts
      pure (k, srcPats, subTree)

    -- Replace non-PVar patterns with fresh variables so that conBranchToEqn
    -- can always extend the occMap for every child occurrence.
    ensureVar p@(PVar _) = pure p
    ensureVar p = freshPat (patTy p)

    patTy :: Pattern -> Ty
    patTy (PVar i) = idType i
    patTy (PCon k _) = snd (splitTy (idType k))
    patTy _ = word

    buildDefault =
      let defMat = defaultMatrix matrix
          defBacts = defaultBoundActs testOcc matrix bacts
       in if null defMat
            then do
              witPats <- case headCons of
                [] -> mapM freshPat restTys
                (first : _) -> do
                  siblings <- siblingConstructors first
                  let missing = siblings \\ headCons
                  case missing of
                    (k : _) -> do
                      info <- lookupConInfo (idName k)
                      fieldPats <- mapM freshPat (conFieldTypes info)
                      pure [PCon k fieldPats]
                    [] -> mapM freshPat restTys
              ctx <- askWarnCtx
              tell [NonExhaustive ctx witPats]
              pure (Just Fail)
            else Just <$> compileMatrix restTys restOccs defMat defBacts

buildLitSwitch ::
  Occurrence ->
  [Occurrence] ->
  [Ty] ->
  PatternMatrix ->
  [BoundAction] ->
  [Literal] ->
  CompilerM DecisionTree
buildLitSwitch testOcc restOccs restTys matrix bacts headLits = do
  branches <- mapM buildBranch headLits
  let defMat = defaultMatrix matrix
      defBacts = defaultBoundActs testOcc matrix bacts
  mDefault <-
    if null defMat
      then do
        pats <- mapM freshPat restTys
        ctx <- askWarnCtx
        tell [NonExhaustive ctx pats]
        pure (Just Fail)
      else Just <$> compileMatrix restTys restOccs defMat defBacts
  pure (LitSwitch testOcc branches mDefault)
  where
    buildBranch lit = do
      let specMat = specializeLit lit matrix
          specBacts = litSpecializedBoundActs lit testOcc matrix bacts
      (lit,) <$> compileMatrix restTys restOccs specMat specBacts

instance Compile (Exp Id) where
  compile v@(Var _) =
    pure v
  compile (Con c es) =
    Con c <$> compile es
  compile (FieldAccess me n) =
    flip FieldAccess n <$> compile me
  compile l@(Lit _) =
    pure l
  compile (Call me f es) =
    Call <$> compile me <*> pure f <*> compile es
  compile (Lam ps bd mt) =
    Lam ps <$> pushCtx ("lambda (" ++ intercalate ", " (map pretty ps) ++ ")") (compile bd) <*> pure mt
  compile (TyExp e t) =
    flip TyExp t <$> compile e
  compile (Cond e1 e2 e3) =
    Cond <$> compile e1 <*> compile e2 <*> compile e3
  compile (Indexed e1 e2) =
    Indexed <$> compile e1 <*> compile e2

instance Compile (Instance Id) where
  compile (Instance d vs ps n ts t funs) =
    Instance d vs ps n ts t
      <$> pushCtx ("instance " ++ pretty t ++ " : " ++ pretty n) (compile funs)

-- compiling a decision tree into a match

-- Convert a decision tree to a statement body (list of statements).
-- Avoids wrapping multi-statement leaves in a fake Match node.
treeToBody :: OccMap -> DecisionTree -> CompilerM [Stmt Id]
treeToBody _ Fail = pure []
treeToBody occMap (Leaf varBinds stmts) = do
  subst <- buildSubst occMap varBinds
  pure (map (substStmt subst) stmts)
treeToBody occMap tree = (: []) <$> treeToStmt occMap tree

treeToStmt :: OccMap -> DecisionTree -> CompilerM (Stmt Id)
treeToStmt _ Fail = pure (Match [] [])
treeToStmt occMap (Leaf varBinds stmts) = do
  subst <- buildSubst occMap varBinds
  case map (substStmt subst) stmts of
    [s] -> pure s
    ss -> pure (Match [] [([], ss)])
treeToStmt occMap (Switch occ branches mDef) = do
  scrutinee <- occToExp occMap occ
  eqns <- mapM (conBranchToEqn occMap occ) branches
  defEqns <- case mDef of
    Nothing -> pure []
    Just def -> do
      body <- treeToBody occMap def
      ty <- scrutineeType scrutinee
      v <- freshPat ty
      pure [([v], body)]
  pure (Match [scrutinee] (eqns ++ defEqns))
treeToStmt occMap (LitSwitch occ branches mDef) = do
  scrutinee <- occToExp occMap occ
  eqns <- mapM (litBranchToEqn occMap) branches
  defEqns <- case mDef of
    Nothing -> pure []
    Just def -> do
      body <- treeToBody occMap def
      ty <- scrutineeType scrutinee
      v <- freshPat ty
      pure [([v], body)]
  pure (Match [scrutinee] (eqns ++ defEqns))

-- Build a substitution map from pattern variables to their occurrence expressions.
buildSubst :: OccMap -> [(Id, Occurrence)] -> CompilerM (Map Id (Exp Id))
buildSubst occMap varBinds = do
  pairs <- mapM (\(v, occ) -> (v,) <$> occToExp occMap occ) varBinds
  pure (Map.fromList pairs)

-- Substitute pattern variables in a statement using the given map.
substStmt :: Map Id (Exp Id) -> Stmt Id -> Stmt Id
substStmt subst = everywhere (mkT go)
  where
    go e@(Var v) = Map.findWithDefault e v subst
    go e = e

occToExp :: OccMap -> Occurrence -> CompilerM (Exp Id)
occToExp occMap occ =
  case Map.lookup occ occMap of
    Just e -> pure e
    Nothing -> throwError ("PANIC! occToExp: occurrence " ++ show occ ++ " not in map")

conBranchToEqn :: OccMap -> Occurrence -> (Id, [Pattern], DecisionTree) -> CompilerM (PatternRow, Action)
conBranchToEqn occMap occ (k, srcPats, tree) = do
  let childOccs' = [occ ++ [j] | j <- [0 .. length srcPats - 1]]
      childExps = [Var fv | (PVar fv) <- srcPats]
      occMap' = Map.union (Map.fromList (zip childOccs' childExps)) occMap
  body <- treeToBody occMap' tree
  pure ([PCon k srcPats], body)

litBranchToEqn :: OccMap -> (Literal, DecisionTree) -> CompilerM (PatternRow, Action)
litBranchToEqn occMap (lit, tree) = do
  body <- treeToBody occMap tree
  pure ([PLit lit], body)

-- inhabitance tests

inhabitsM :: PatternMatrix -> [Ty] -> CompilerM (Maybe [Pattern])
inhabitsM [] tys = do
  pats <- mapM freshPat tys
  pure (Just pats)
inhabitsM _ [] = pure Nothing
inhabitsM matrix (ty : restTys) = do
  let firstCol = [p | (p : _) <- matrix]
      headCons = nub (mapMaybe patHeadCon firstCol)
      headLits = nub (mapMaybe patHeadLit firstCol)
  case (headCons, headLits) of
    (c : cs, _) -> inhabitsConCol matrix restTys (c : cs)
    ([], _ : _) -> inhabitsLitCol matrix ty restTys
    ([], []) -> do
      p <- freshPat ty
      fmap (p :) <$> inhabitsM (defaultMatrix matrix) restTys

inhabitsConCol :: PatternMatrix -> [Ty] -> [Id] -> CompilerM (Maybe [Pattern])
inhabitsConCol matrix restTys (firstCon : otherCons) = do
  siblings <- siblingConstructors firstCon
  let headCons = firstCon : otherCons
      missing = filter (\s -> idName s `notElem` map idName headCons) siblings
  case missing of
    (k : _) -> do
      info <- lookupConInfo (idName k)
      let fieldTys = conFieldTypes info
      mw <- inhabitsM (defaultMatrix matrix) restTys
      case mw of
        Nothing -> pure Nothing
        Just rest -> do
          sub <- mapM freshPat fieldTys
          pure . Just $ PCon k sub : rest
    [] -> do
      results <- mapM searchBranch headCons
      pure (listToMaybe (catMaybes results))
  where
    searchBranch k = do
      info <- lookupConInfo (idName k)
      let fieldTys = conFieldTypes info
      specMat <- specialize k fieldTys matrix
      mw <- inhabitsM specMat (fieldTys ++ restTys)
      case mw of
        Nothing -> pure Nothing
        Just ws ->
          let (sub, rest) = splitAt (length fieldTys) ws
           in pure . Just $ PCon k sub : rest
inhabitsConCol _ _ [] = pure Nothing

inhabitsLitCol :: PatternMatrix -> Ty -> [Ty] -> CompilerM (Maybe [Pattern])
inhabitsLitCol matrix ty restTys = do
  pat <- freshPat ty
  fmap (pat :) <$> inhabitsM (defaultMatrix matrix) restTys

-- definition of a monad

-- Context attached to warnings: stack of pretty-printed descriptions of the
-- syntax nodes being traversed, from outermost to innermost.
type WarnCtx = [String]

type CompilerM a =
  ReaderT (TypeEnv, WarnCtx) (ExceptT String (WriterT [Warning] (StateT Int IO))) a

runCompilerM :: TypeEnv -> CompilerM a -> IO (Either String (a, [Warning]))
runCompilerM env m =
  do
    ((r, ws), _) <- runStateT (runWriterT (runExceptT (runReaderT m (env, [])))) 0
    case r of
      Left err -> pure $ Left err
      Right t -> pure $ Right (t, ws)

askTypeEnv :: CompilerM TypeEnv
askTypeEnv = asks fst

askWarnCtx :: CompilerM WarnCtx
askWarnCtx = asks snd

pushCtx :: String -> CompilerM a -> CompilerM a
pushCtx s = local (\(te, ctx) -> (te, ctx ++ [s]))

lookupConInfo :: Name -> CompilerM ConInfo
lookupConInfo k = do
  env <- askTypeEnv
  case Map.lookup k env of
    Just info -> pure info
    Nothing -> undefinedConstructorError k

siblingConstructors :: Id -> CompilerM [Id]
siblingConstructors k = do
  env <- askTypeEnv
  info <- lookupConInfo (idName k)
  let rt = conReturnType info
  pure [idFromInfo kid ci | (kid, ci) <- Map.toList env, conReturnType ci == rt]

idFromInfo :: Name -> ConInfo -> Id
idFromInfo n info =
  Id
    n
    ( funtype
        (conFieldTypes info)
        (conReturnType info)
    )

inc :: CompilerM Int
inc = do
  n <- get
  put (n + 1)
  pure n

freshPat :: Ty -> CompilerM (Pat Id)
freshPat t = do
  n <- inc
  let v = Name $ "v" ++ show n
  pure (PVar (Id v t))

isComplete :: [Id] -> CompilerM Bool
isComplete [] = pure False
isComplete ks@(first : _) = do
  siblings <- siblingConstructors first
  let siblingNames = map idName siblings
      ksNames = map idName ks
  pure (null (siblingNames \\ ksNames))

-- some types used by the algorithm

type Occurrence = [Int]

type OccMap = Map Occurrence (Exp Id)

type Action = [Stmt Id]

-- Per-row accumulated bindings (var → occurrence) collected as PVar patterns
-- are consumed by specialization steps, paired with the row's action.
type BoundAction = ([(Id, Occurrence)], Action)

type Pattern = Pat Id

type PatternRow = [Pattern]

type PatternMatrix = [PatternRow]

data DecisionTree
  = Leaf [(Id, Occurrence)] Action -- var-occ bindings to substitute before running action
  | Fail
  | Switch Occurrence [(Id, [Pattern], DecisionTree)] (Maybe DecisionTree)
  | LitSwitch Occurrence [(Literal, DecisionTree)] (Maybe DecisionTree)
  deriving (Eq, Show)

-- data constructor information and environment

type TypeEnv = Map Name ConInfo

data ConInfo
  = ConInfo
  { conFieldTypes :: [Ty],
    conReturnType :: Ty
  }
  deriving (Show)

initialTypeEnv :: CompUnit Id -> TypeEnv
initialTypeEnv (CompUnit _ ds) =
  foldr step primEnv ds
  where
    step (TDataDef dt) ac = addDataTyInfo dt ac
    step (TMutualDef ds1) ac = foldr step ac ds1
    step _ ac = ac

primEnv :: TypeEnv
primEnv =
  Map.fromList
    [ (unitName, unitConInfo),
      (pairName, pairConInfo),
      (inlName, inlConInfo),
      (inrName, inrConInfo),
      (trueName, trueConInfo),
      (falseName, falseConInfo)
    ]

unitName :: Name
unitName = Name "()"

unitConInfo :: ConInfo
unitConInfo = ConInfo [] unit

pairName :: Name
pairName = Name "pair"

pairConInfo :: ConInfo
pairConInfo = ConInfo [at, bt] (pairTy at bt)

inlConInfo :: ConInfo
inlConInfo = ConInfo [at] (inlTy at bt)

inrConInfo :: ConInfo
inrConInfo = ConInfo [bt] (inlTy at bt)

trueConInfo :: ConInfo
trueConInfo = ConInfo [] boolTy

falseConInfo :: ConInfo
falseConInfo = ConInfo [] boolTy

addDataTyInfo :: DataTy -> TypeEnv -> TypeEnv
addDataTyInfo (DataTy n vs cons) env =
  foldr (addConstructor res) env cons
  where
    res = TyCon n (map TyVar vs)

addConstructor :: Ty -> Constr -> TypeEnv -> TypeEnv
addConstructor ty (Constr n ts) =
  Map.insert n (ConInfo ts ty)

-- matrix specialization

specialize :: Id -> [Ty] -> PatternMatrix -> CompilerM PatternMatrix
specialize k fieldTys matrix =
  do
    pats <- mapM freshPat fieldTys
    pure $ mapMaybe (specRow k pats) matrix

specRow :: Id -> [Pattern] -> PatternRow -> Maybe PatternRow
specRow _ _ [] = Nothing
specRow k pats (p : rest) =
  case p of
    PCon k' ps
      | idName k' == idName k -> Just (ps ++ rest)
      | otherwise -> Nothing
    PVar _ -> Just (pats ++ rest)
    PLit _ -> Nothing
    _ -> error "PANIC! Found wildcard in specRow"

specializeLit :: Literal -> PatternMatrix -> PatternMatrix
specializeLit lit = mapMaybe specLitRow
  where
    specLitRow [] = Nothing
    specLitRow (p : rest) =
      case p of
        PLit l
          | l == lit -> Just rest
          | otherwise -> Nothing
        PVar _ -> Just rest
        PCon _ _ -> Nothing
        _ -> error "PANIC! Found wildcard in specializeLit"

-- matrix definitions

defaultMatrix :: PatternMatrix -> PatternMatrix
defaultMatrix = concatMap defaultRow
  where
    defaultRow [] = []
    defaultRow (p : rest) =
      case p of
        PVar _ -> [rest]
        PCon _ _ -> []
        PLit _ -> []
        _ -> error "PANIC! Found wildcard in defaultMatrix"

specializedBoundActs :: Id -> Occurrence -> PatternMatrix -> [BoundAction] -> [BoundAction]
specializedBoundActs k testOcc rows bacts =
  [ (addVarBinding row binds, a)
    | (row, (binds, a)) <- zip rows bacts,
      rowMatchesCon row
  ]
  where
    rowMatchesCon [] = False
    rowMatchesCon (p : _) = case p of
      PCon k' _ -> idName k' == idName k
      PVar _ -> True
      PLit _ -> False
      PWildcard -> error "PANIC! Found wildcard in specializedBoundActs"
    addVarBinding (PVar v : _) binds = binds ++ [(v, testOcc)]
    addVarBinding _ binds = binds

defaultBoundActs :: Occurrence -> PatternMatrix -> [BoundAction] -> [BoundAction]
defaultBoundActs testOcc rows bacts =
  [ (addVarBinding row binds, a)
    | (row, (binds, a)) <- zip rows bacts,
      case row of
        (p : _) -> isVarPat p
        [] -> False
  ]
  where
    addVarBinding (PVar v : _) binds = binds ++ [(v, testOcc)]
    addVarBinding _ binds = binds

litSpecializedBoundActs :: Literal -> Occurrence -> PatternMatrix -> [BoundAction] -> [BoundAction]
litSpecializedBoundActs lit testOcc rows bacts =
  [ (addVarBinding row binds, a)
    | (row, (binds, a)) <- zip rows bacts,
      rowMatchesLit row
  ]
  where
    rowMatchesLit [] = False
    rowMatchesLit (p : _) = case p of
      PLit l -> l == lit
      PVar _ -> True
      _ -> False
    addVarBinding (PVar v : _) binds = binds ++ [(v, testOcc)]
    addVarBinding _ binds = binds

-- column selection heuristic

necessityScore :: [Pattern] -> Int
necessityScore = length . filter (not . isVarPat)

selectColumn :: [Occurrence] -> PatternMatrix -> Int
selectColumn _ [] = 0
selectColumn occs matrix =
  case map necessityScore (transpose matrix) of
    [] -> 0
    scores ->
      let best = maximum scores
          candidates = [i | (i, s) <- zip [0 ..] scores, s == best]
          rank i = length (occs !! i)
       in minimumBy (comparing rank) candidates

reorderList :: Int -> [a] -> [a]
reorderList i xs =
  let (before, after) = splitAt i xs
   in case after of
        [] -> xs
        (x : rest) -> x : before ++ rest

childOccs :: Occurrence -> Int -> [Occurrence]
childOccs occ n = [occ ++ [j] | j <- [0 .. n - 1]]

-- auxiliar functions

scrutineeType :: Exp Id -> CompilerM Ty
scrutineeType (Var i) = pure (idType i)
scrutineeType (Con i _) = pure (snd (splitTy (idType i)))
scrutineeType (Lit (IntLit _)) = pure word
scrutineeType (Lit (StrLit _)) = pure string
scrutineeType (Call _ i _) = pure (snd (splitTy (idType i)))
scrutineeType (Lam args _body (Just tb)) = pure (funtype (map typeOfParam args) tb)
scrutineeType (Lam _ _ Nothing) =
  throwError
    "scrutineeType: lambda scrutinee has no return-type annotation"
scrutineeType (Cond _ _ e) = scrutineeType e
scrutineeType (TyExp _ ty) = pure ty
scrutineeType (FieldAccess _ n) = pure (idType n)
scrutineeType (Indexed earr _) =
  do
    tarr <- scrutineeType earr
    case tarr of
      (_ :-> tres) -> pure tres
      _ ->
        throwError
          "scrutineeType: index expression scrutinee has no type annotation"

typeOfParam :: Param Id -> Ty
typeOfParam (Typed i _t) = idType i
typeOfParam (Untyped i) = idType i

isVarPat :: Pattern -> Bool
isVarPat (PVar _) = True
isVarPat _ = False

patHeadCon :: Pattern -> Maybe Id
patHeadCon (PCon k _) = Just k
patHeadCon _ = Nothing

patHeadLit :: Pattern -> Maybe Literal
patHeadLit (PLit l) = Just l
patHeadLit _ = Nothing

-- warnings

data Warning
  = RedundantClause WarnCtx PatternRow Action
  | NonExhaustive WarnCtx [Pattern]
  deriving (Eq, Show)

isNonExaustive :: Warning -> Bool
isNonExaustive (NonExhaustive _ _) = True
isNonExaustive _ = False

-- Pretty-print a warning

showWarning :: Warning -> String
showWarning (RedundantClause ctx row blk) =
  unwords ["Warning: Clause ", "(", pretty (row, blk), ") is redundant.", showWarnCtx ctx]
showWarning (NonExhaustive ctx pats) =
  unwords ["Non-exhaustive pattern match. Missing case:", showRow $ nub pats, showWarnCtx ctx]

showWarnCtx :: WarnCtx -> String
showWarnCtx [] = ""
showWarnCtx ctx = "\n  in " ++ intercalate "\n  in " (reverse ctx)

showRow :: [Pattern] -> String
showRow = intercalate ", " . map pretty

-- error messages

undefinedConstructorError :: Name -> CompilerM a
undefinedConstructorError n =
  throwError $
    unlines
      [ "PANIC!",
        "Constructor:" ++ pretty n,
        "is not defined!"
      ]
