module Solcore.Desugarer.DecisionTreeCompiler where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
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
    Contract n vs <$> local (Map.union env') (compile ds)
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
    FunDef sig <$> compile bd

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
    let scrutTys = map scrutineeType es'
        occs = [[i] | i <- [0 .. length es' - 1]]
        matrix = map fst eqns'
        actions = map snd eqns'
    tree <- compileMatrix scrutTys occs matrix actions
    treeToStmt es' tree

compileEqn :: Equation Id -> CompilerM (Equation Id)
compileEqn (pats, stmts) = (pats,) <$> compile stmts

compileMatrix ::
  [Ty] ->
  [Occurrence] ->
  PatternMatrix ->
  [Action] ->
  CompilerM DecisionTree
compileMatrix tys _ [] _ = do
  pats <- mapM freshPat tys
  unless (null pats) $ tell [NonExhaustive pats]
  pure Fail
-- First row is all wildcards/variables: unconditional match
compileMatrix tys _ (firstRow : restRows) (firstAct : _)
  | all isVarPat firstRow = do
      mw <-
        if null tys
          then pure (Just [])
          else inhabitsM restRows tys
      let redundant = isNothing mw
          warns = [RedundantClause firstRow firstAct | redundant]
      tell warns
      pure (Leaf firstAct)
-- General case: at least one constructor or literal in the first row
compileMatrix tys occs matrix acts = do
  let col = selectColumn matrix
      matrix' = reorderList col matrix
      occs' = reorderList col occs
      tys' = reorderList col tys
  -- Pattern-match on the reordered lists to avoid head/tail
  case (tys', occs', matrix') of
    (_ : restTys, testOcc : restOccs, _) -> do
      let firstCol = [p | (p : _) <- matrix']
          headCons = nub (mapMaybe patHeadCon firstCol)
          headLits = nub (mapMaybe patHeadLit firstCol)
      case (headCons, headLits) of
        (c : cs, _) ->
          buildConSwitch testOcc restOccs restTys matrix' acts (c : cs)
        ([], ls@(_ : _)) ->
          buildLitSwitch testOcc restOccs restTys matrix' acts ls
        ([], []) ->
          -- All wildcards after reordering: strip column and continue
          compileMatrix restTys restOccs (defaultMatrix matrix') (defaultActions matrix' acts)
    -- Unreachable: tys, occs, and matrix are parallel and all non-empty here
    _ -> pure Fail

buildConSwitch ::
  Occurrence ->
  [Occurrence] ->
  [Ty] ->
  PatternMatrix ->
  [Action] ->
  [Id] ->
  CompilerM DecisionTree
buildConSwitch testOcc restOccs restTys matrix acts headCons = do
  branches <- mapM buildBranch $ headCons
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
          specActs = specializedActions k matrix acts
      specMat <- specialize k fieldTys matrix
      (k,) <$> compileMatrix newTys newOccs specMat specActs

    buildDefault =
      let defMat = defaultMatrix matrix
          defActs = defaultActions matrix acts
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
              tell [NonExhaustive witPats]
              pure (Just Fail)
            else Just <$> compileMatrix restTys restOccs defMat defActs

buildLitSwitch ::
  Occurrence ->
  [Occurrence] ->
  [Ty] ->
  PatternMatrix ->
  [Action] ->
  [Literal] ->
  CompilerM DecisionTree
buildLitSwitch testOcc restOccs restTys matrix acts headLits = do
  branches <- mapM buildBranch $ headLits
  let defMat = defaultMatrix matrix
      defActs = defaultActions matrix acts
  mDefault <-
    if null defMat
      then do
        pats <- mapM freshPat restTys
        tell [NonExhaustive pats]
        pure (Just Fail)
      else Just <$> compileMatrix restTys restOccs defMat defActs
  pure (LitSwitch testOcc branches mDefault)
  where
    buildBranch lit = do
      let specMat = specializeLit lit matrix
          specActs = litSpecializedActs lit matrix acts
      (lit,) <$> compileMatrix restTys restOccs specMat specActs

    litSpecializedActs lit rows as_ =
      [ a | (row, a) <- zip rows as_, case row of
                                        [] -> False
                                        (p : _) -> case p of
                                          PLit l -> l == lit
                                          PVar _ -> True
                                          PCon _ _ -> False
                                          _ -> error $ "PANIC! Found wildcard in specializeLit"
      ]

instance Compile (Exp Id) where
  compile v@(Var _) =
    pure v
  compile (Con c es) =
    Con c <$> compile es
  compile (FieldAccess me n) =
    (flip FieldAccess n) <$> compile me
  compile l@(Lit _) =
    pure l
  compile (Call me f es) =
    Call <$> compile me <*> pure f <*> compile es
  compile (Lam ps bd mt) =
    Lam ps <$> compile bd <*> pure mt
  compile (TyExp e t) =
    flip TyExp t <$> compile e
  compile (Cond e1 e2 e3) =
    Cond <$> compile e1 <*> compile e2 <*> compile e3
  compile (Indexed e1 e2) =
    Indexed <$> compile e1 <*> compile e2

instance Compile (Instance Id) where
  compile (Instance d vs ps n ts t funs) =
    Instance d vs ps n ts t <$> compile funs

-- compiling a decision tree into a match

treeToStmt :: [Exp Id] -> DecisionTree -> CompilerM (Stmt Id)
treeToStmt _ Fail =
  -- An empty Match signals non-exhaustive patterns at runtime.
  pure (Match [] [])
treeToStmt _ (Leaf stmts) =
  -- Wrap multi-statement bodies in a block; single statements unwrap cleanly.
  case stmts of
    [s] -> pure s
    _ -> pure (Match [] [([], stmts)])
treeToStmt es (Switch occ branches mDef) = do
  scrutinee <- occToExp es occ
  eqns <- mapM (conBranchToEqn es) branches
  defEqns <- case mDef of
    Nothing -> pure []
    Just def -> do
      s <- treeToStmt es def
      pure [([], [s])]
  pure (Match [scrutinee] (eqns ++ defEqns))
treeToStmt es (LitSwitch occ branches mDef) = do
  scrutinee <- occToExp es occ
  eqns <- mapM (litBranchToEqn es) branches
  defEqns <- case mDef of
    Nothing -> pure []
    Just def -> do
      s <- treeToStmt es def
      pure [([], [s])]
  pure (Match [scrutinee] (eqns ++ defEqns))

occToExp :: [Exp Id] -> Occurrence -> CompilerM (Exp Id)
occToExp es occ = case occ of
  [] ->
    -- Empty occurrence: return the sole scrutinee if there is one
    case es of
      (e : _) -> pure e
      [] -> throwError "occToExp: empty occurrence with no scrutinees"
  [i] ->
    case drop i es of
      (e : _) -> pure e
      [] ->
        throwError
          ( "occToExp: index "
              ++ show i
              ++ " out of range for "
              ++ show (length es)
              ++ " scrutinees"
          )
  (i : _) -> occToExp es [i]

conBranchToEqn :: [Exp Id] -> (Id, DecisionTree) -> CompilerM (PatternRow, Action)
conBranchToEqn es (k, tree) = do
  info <- lookupConInfo (idName k)
  -- Generate one fresh variable pattern per field so the sub-expressions
  -- introduced by childOccs can be referred to in the nested tree.
  fieldPats <- mapM freshPat (conFieldTypes info)
  s <- treeToStmt es tree
  pure ([PCon k fieldPats], [s])

litBranchToEqn :: [Exp Id] -> (Literal, DecisionTree) -> CompilerM (PatternRow, Action)
litBranchToEqn es (lit, tree) = do
  s <- treeToStmt es tree
  pure ([PLit lit], [s])

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
      mw <- inhabitsM (defaultMatrix matrix) (fieldTys ++ restTys)
      case mw of
        Nothing -> pure Nothing
        Just ws ->
          let (sub, rest) = splitAt (length fieldTys) ws
           in pure . Just $ PCon k sub : rest
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

type CompilerM a =
  ReaderT TypeEnv (ExceptT String (WriterT [Warning] (StateT Int IO))) a

runCompilerM :: TypeEnv -> CompilerM a -> IO (Either String (a, [Warning]))
runCompilerM env m =
  do
    ((r, ws), _) <- runStateT (runWriterT (runExceptT (runReaderT m env))) 0
    case r of
      Left err -> pure $ Left err
      Right t -> pure $ Right (t, ws)

lookupConInfo :: Name -> CompilerM ConInfo
lookupConInfo k = do
  env <- ask
  case Map.lookup k env of
    Just info -> pure info
    Nothing -> undefinedConstructorError k

siblingConstructors :: Id -> CompilerM [Id]
siblingConstructors k = do
  env <- ask
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

type Action = [Stmt Id]

type Pattern = Pat Id

type PatternRow = [Pattern]

type PatternMatrix = [PatternRow]

data DecisionTree
  = Leaf Action
  | Fail
  | Switch Occurrence [(Id, DecisionTree)] (Maybe DecisionTree)
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
addConstructor ty (Constr n ts) env =
  Map.insert n (ConInfo ts ty) env

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
      | k' == k -> Just (ps ++ rest)
      | otherwise -> Nothing
    PVar _ -> Just (pats ++ rest)
    PLit _ -> Nothing
    _ -> error $ "PANIC! Found wildcard in specRow"

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
        _ -> error $ "PANIC! Found wildcard in specializeLit"

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
        _ -> error $ "PANIC! Found wildcard in defaultMatrix"

specializedActions :: Id -> PatternMatrix -> [a] -> [a]
specializedActions k rows acts =
  [ a | (row, a) <- zip rows acts, case row of
                                     [] -> False
                                     (p : _) -> case p of
                                       PCon k' _ -> k' == k
                                       PVar _ -> True
                                       PLit _ -> False
                                       PWildcard -> error $ "PANIC! Found wildcard in specializedActions"
  ]

defaultActions :: PatternMatrix -> [a] -> [a]
defaultActions rows acts =
  [ a | (row, a) <- zip rows acts, case row of
                                     [] -> False
                                     (p : _) -> isVarPat p
  ]

-- column selection heuristic

necessityScore :: [Pattern] -> Int
necessityScore = length . filter (not . isVarPat)

selectColumn :: PatternMatrix -> Int
selectColumn [] = 0
selectColumn matrix =
  case map necessityScore (transpose matrix) of
    [] -> 0
    (s : ss) ->
      let best = foldl max s ss -- safe: foldl max with a seed, never empty
          scores = s : ss
       in length (takeWhile (/= best) scores)

reorderList :: Int -> [a] -> [a]
reorderList i xs =
  let (before, after) = splitAt i xs
   in case after of
        [] -> xs
        (x : rest) -> x : before ++ rest

childOccs :: Occurrence -> Int -> [Occurrence]
childOccs occ n = [occ ++ [j] | j <- [0 .. n - 1]]

-- auxiliar functions

scrutineeType :: Exp Id -> Ty
scrutineeType (Var i) = idType i
scrutineeType (Con i _) = snd (splitTy (idType i))
scrutineeType (Lit (IntLit _)) = word
scrutineeType (Lit (StrLit _)) = string
scrutineeType (Call Nothing i _) = snd (splitTy (idType i))
scrutineeType (Lam args _body (Just tb)) = funtype tas tb
  where
    tas = map typeOfParam args
scrutineeType (Cond _ _ e) = scrutineeType e
scrutineeType (TyExp _ ty) = ty
scrutineeType e = error $ "scrutineeType: " ++ show e

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
  = RedundantClause PatternRow Action
  | NonExhaustive [Pattern]
  deriving (Eq, Show)

isNonExaustive :: Warning -> Bool
isNonExaustive (NonExhaustive _) = True
isNonExaustive _ = False

-- Pretty-print a warning

showWarning :: Warning -> String
showWarning (RedundantClause row blk) =
  unwords ["Warning: Clause ", "(", pretty (row, blk), ") is redundant."]
showWarning (NonExhaustive pats) =
  unwords ["Non-exhaustive pattern match. Missing case:", showRow $ nub pats]

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
