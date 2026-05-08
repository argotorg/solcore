module Solcore.Frontend.TypeInference.TcResolution where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad hiding (insts)
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Pipeline.Options (TypeClassResolutionMode (..), optTypeClassResolution)
import Solcore.Primitives.Primitives (unit)

data TypeClassResolver
  = TypeClassResolver
  { resolverName :: String,
    resolverSuperPreds :: ClassTable -> [Pred] -> [Pred],
    resolverEntail :: ClassTable -> InstTable -> [Pred] -> Pred -> Bool,
    resolverToHnfs :: Int -> [Pred] -> TcM [Pred]
  }

askTypeClassResolver :: TcM TypeClassResolver
askTypeClassResolver =
  do
    opts <- getTcOpts
    pure $
      case optTypeClassResolution opts of
        LegacyResolution -> legacyTypeClassResolver
        TabledResolution -> tabledTypeClassResolver

superPredsM :: ClassTable -> [Pred] -> TcM [Pred]
superPredsM ctable ps =
  do
    resolver <- askTypeClassResolver
    pure (resolverSuperPreds resolver ctable ps)

entailM :: ClassTable -> InstTable -> [Pred] -> Pred -> TcM Bool
entailM ctable itable qs p =
  do
    resolver <- askTypeClassResolver
    pure (resolverEntail resolver ctable itable qs p)

toHnfs :: Int -> [Pred] -> TcM [Pred]
toHnfs depth ps =
  do
    resolver <- askTypeClassResolver
    resolverToHnfs resolver depth ps

legacyTypeClassResolver :: TypeClassResolver
legacyTypeClassResolver =
  TypeClassResolver
    { resolverName = "legacy",
      resolverSuperPreds = \ctable -> concatMap (bySuperM ctable),
      resolverEntail = legacyEntail,
      resolverToHnfs = legacyToHnfs
    }

tabledTypeClassResolver :: TypeClassResolver
tabledTypeClassResolver =
  TypeClassResolver
    { resolverName = "tabled",
      resolverSuperPreds = \ctable -> concatMap (bySuperM ctable),
      resolverEntail = tabledEntail,
      resolverToHnfs = tabledToHnfs
    }

-- converting to head normal forms

newtype TableKey
  = TableKey Pred
  deriving (Eq, Ord, Show)

data ResolutionAnswer
  = ResolutionAnswer
  { answerGoal :: Pred,
    answerSubst :: Subst,
    answerResidual :: [Pred]
  }
  deriving (Eq, Show)

data Waiter
  = Waiter
  { waiterTarget :: ConsumerTarget,
    waiterSubst :: Subst,
    waiterResidual :: [Pred],
    waiterGoal :: Pred,
    waiterRest :: [Pred]
  }
  deriving (Show)

data ConsumerTarget
  = RootConsumer
  | AnswerConsumer TableKey Pred
  deriving (Eq, Ord, Show)

data GeneratorNode
  = GeneratorNode
  { generatorKey :: TableKey,
    generatorGoal :: Pred,
    generatorSubst :: Subst,
    generatorInstances :: [Inst],
    generatorIndex :: Int,
    generatorTriedDefault :: Bool
  }
  deriving (Show)

data TableEntry
  = TableEntry
  { entryWaiters :: [Waiter],
    entryAnswers :: [ResolutionAnswer],
    entryExhausted :: Bool
  }
  deriving (Show)

data TabledSearch
  = TabledSearch
  { searchGeneratorStack :: [GeneratorNode],
    searchResumeStack :: [(Waiter, ResolutionAnswer)],
    searchTableEntries :: Map.Map TableKey TableEntry,
    searchRootAnswers :: [ResolutionAnswer]
  }
  deriving (Show)

type TabledM a = StateT TabledSearch (StateT TcEnv (ExceptT String IO)) a

emptyTabledSearch :: TabledSearch
emptyTabledSearch =
  TabledSearch
    { searchGeneratorStack = [],
      searchResumeStack = [],
      searchTableEntries = Map.empty,
      searchRootAnswers = []
    }

tabledToHnfs :: Int -> [Pred] -> TcM [Pred]
tabledToHnfs depth ps = do
  baseSubst <- getSubst
  (_, finalSearch) <-
    runStateT
      ( do
          newConsumer RootConsumer baseSubst ps
          searchLoop depth
      )
      emptyTabledSearch
  case searchRootAnswers finalSearch of
    [] -> pure ps
    (answer : _) -> do
      putSubst (answerSubst answer)
      withCurrentSubst (answerResidual answer)

searchLoop :: Int -> TabledM ()
searchLoop depth
  | depth <= 0 = do
      st <- get
      lift $ notEnoughFuel (pendingGoals st)
  | otherwise = do
      st <- get
      case searchResumeStack st of
        ((waiter, answer) : rest) -> do
          put st {searchResumeStack = rest}
          resumeConsumer waiter answer
          searchLoop (depth - 1)
        [] ->
          case searchGeneratorStack st of
            [] -> pure ()
            _ -> do
              generateStep
              searchLoop (depth - 1)

pendingGoals :: TabledSearch -> [Pred]
pendingGoals st =
  map generatorGoal (searchGeneratorStack st)
    ++ [waiterGoal waiter | (waiter, _) <- searchResumeStack st]

newConsumer :: ConsumerTarget -> Subst -> [Pred] -> TabledM ()
newConsumer target consumerSubst =
  newConsumerWithResidual target consumerSubst []

newConsumerWithResidual :: ConsumerTarget -> Subst -> [Pred] -> [Pred] -> TabledM ()
newConsumerWithResidual target consumerSubst residualAcc goals0 = do
  lift $ putSubst consumerSubst
  lift $ info [">> Consuming (tabled):", pretty goals0]
  goals1 <- lift $ elimEqualities goals0
  goals <- lift $ withCurrentSubst goals1
  case goals of
    [] -> addAnswer target residualAcc
    (p : ps) ->
      case p of
        InCls {} -> newSubgoal target residualAcc p ps
        t1 :~: t2 -> do
          lift $ putSubst consumerSubst
          lift $ info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved)"]
          s <- lift $ unify t1 t2
          lift $ info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved: ", pretty s, ")"]
          s' <- lift getSubst
          ps' <- lift $ withCurrentSubst ps
          newConsumerWithResidual target s' residualAcc ps'

newSubgoal :: ConsumerTarget -> [Pred] -> Pred -> [Pred] -> TabledM ()
newSubgoal target residualAcc p rest = do
  currentSubst <- lift getSubst
  let key = tableKey p
      waiter = Waiter target currentSubst residualAcc p rest
  mEntry <- gets (Map.lookup key . searchTableEntries)
  case mEntry of
    Nothing -> do
      insts <- lift $ askInstEnv (predName p)
      let entry = TableEntry [] [] False
          generator = GeneratorNode key p currentSubst insts 0 False
      modify
        ( \st ->
            st
              { searchTableEntries = Map.insert key entry (searchTableEntries st),
                searchGeneratorStack = generator : searchGeneratorStack st
              }
        )
      registerWaiter key waiter
    Just entry -> do
      mapM_ (enqueueResume waiter) (entryAnswers entry)
      if entryExhausted entry
        then pure ()
        else registerWaiter key waiter

registerWaiter :: TableKey -> Waiter -> TabledM ()
registerWaiter key waiter =
  modifyTableEntry key $ \entry ->
    entry {entryWaiters = waiter : entryWaiters entry}

enqueueResume :: Waiter -> ResolutionAnswer -> TabledM ()
enqueueResume waiter answer =
  modify $ \st ->
    st {searchResumeStack = (waiter, answer) : searchResumeStack st}

resumeConsumer :: Waiter -> ResolutionAnswer -> TabledM ()
resumeConsumer waiter answer = do
  lift $ putSubst (waiterSubst waiter)
  case instantiateAnswer answer (waiterGoal waiter) of
    Nothing ->
      pure ()
    Just (answerSubst', residual) -> do
      _ <- lift $ extSubst answerSubst'
      subst' <- lift getSubst
      residual' <- lift $ withCurrentSubst residual
      rest' <- lift $ withCurrentSubst (waiterRest waiter)
      let residualAcc = waiterResidual waiter ++ residual'
      newConsumerWithResidual (waiterTarget waiter) subst' residualAcc rest'

generateStep :: TabledM ()
generateStep = do
  st <- get
  case searchGeneratorStack st of
    [] -> pure ()
    (generator : rest) ->
      if generatorIndex generator < length (generatorInstances generator)
        then do
          let inst = generatorInstances generator !! generatorIndex generator
              generator' = generator {generatorIndex = generatorIndex generator + 1}
          put st {searchGeneratorStack = generator' : rest}
          tryInstance generator inst
        else
          if not (generatorTriedDefault generator)
            then do
              let generator' = generator {generatorTriedDefault = True}
              put st {searchGeneratorStack = generator' : rest}
              tryDefaultInstance generator
            else
              finishGenerator generator rest

tryInstance :: GeneratorNode -> Inst -> TabledM ()
tryInstance generator inst = do
  lift $ putSubst (generatorSubst generator)
  case byInst inst (generatorGoal generator) of
    Nothing -> pure ()
    Just (ps, s, i) -> do
      lift $ info [">>> Found tabled instance for:", pretty (generatorGoal generator), "\n>>> Instance:", pretty i, "\n>>> Subst:", pretty s]
      _ <- lift $ extSubst s
      subst' <- lift getSubst
      ps' <- lift $ withCurrentSubst ps
      newConsumer (AnswerConsumer (generatorKey generator) (generatorGoal generator)) subst' ps'

tryDefaultInstance :: GeneratorNode -> TabledM ()
tryDefaultInstance generator = do
  lift $ putSubst (generatorSubst generator)
  denv <- lift getDefaultInstEnv
  insts <- lift $ askInstEnv (predName (generatorGoal generator))
  case proveDefaulting denv insts (generatorGoal generator) of
    Nothing -> pure ()
    Just (_, s) -> do
      lift $ info [">>>> Default instance for:", pretty (generatorGoal generator), " found! (Solved), \n>>> Subst: ", pretty s]
      _ <- lift $ extSubst s
      addAnswer (AnswerConsumer (generatorKey generator) (generatorGoal generator)) []

finishGenerator :: GeneratorNode -> [GeneratorNode] -> TabledM ()
finishGenerator generator rest = do
  st <- get
  let key = generatorKey generator
      entry = fromMaybe emptyTableEntry (Map.lookup key (searchTableEntries st))
  put st {searchGeneratorStack = rest}
  if null (entryAnswers entry)
    then do
      lift $ info [">>>> No tabled instance found for:", pretty (generatorGoal generator)]
      lift $ putSubst (generatorSubst generator)
      addAnswer (AnswerConsumer key (generatorGoal generator)) [generatorGoal generator]
      markExhausted key
    else markExhausted key

addAnswer :: ConsumerTarget -> [Pred] -> TabledM ()
addAnswer RootConsumer residual = do
  answerSubst' <- lift getSubst
  residual' <- lift $ withCurrentSubst residual
  let answer = ResolutionAnswer (InCls (Name "$root") unit []) answerSubst' residual'
  known <- gets (isKnownAnswer answer . searchRootAnswers)
  unless known $
    modify $ \st -> st {searchRootAnswers = searchRootAnswers st ++ [answer]}
addAnswer (AnswerConsumer key goal) residual = do
  answerSubst' <- lift getSubst
  residual' <- lift $ withCurrentSubst residual
  let answer = ResolutionAnswer goal answerSubst' residual'
  entry <- gets (fromMaybe emptyTableEntry . Map.lookup key . searchTableEntries)
  unless (isKnownAnswer answer (entryAnswers entry)) $ do
    modifyTableEntry key $ \entry' ->
      entry' {entryAnswers = entryAnswers entry' ++ [answer]}
    mapM_ (`enqueueResume` answer) (entryWaiters entry)

type AnswerFingerprint = (Pred, [Pred], [Pred])

isKnownAnswer :: ResolutionAnswer -> [ResolutionAnswer] -> Bool
isKnownAnswer answer =
  any
    ( \old ->
        answerFingerprint old == answerFingerprint answer
    )

answerFingerprint :: ResolutionAnswer -> AnswerFingerprint
answerFingerprint answer =
  let (goalAndResidual, st) =
        runState
          (mapM canonicalPredM (answerGoal answer : answerResidual answer))
          emptyCanonicalState
      substEqs =
        sort $
          evalState
            (mapM canonicalPredM (substPreds (relevantSubst answer)))
            st
   in case goalAndResidual of
        goal : residual -> (goal, residual, substEqs)
        [] -> (answerGoal answer, [], substEqs)

relevantSubst :: ResolutionAnswer -> Subst
relevantSubst answer =
  Subst [(v, t) | v <- relevantMetas, Just t <- [lookup v substItems]]
  where
    substItems = unSubst (answerSubst answer)
    roots = mv (answerGoal answer : answerResidual answer)
    relevantMetas = close [] roots

    close seen [] = reverse seen
    close seen (v : vs)
      | v `elem` seen = close seen vs
      | otherwise =
          case lookup v substItems of
            Nothing -> close (v : seen) vs
            Just t -> close (v : seen) (mv t ++ vs)

substPreds :: Subst -> [Pred]
substPreds (Subst xs) =
  [Meta v :~: t | (v, t) <- xs]

markExhausted :: TableKey -> TabledM ()
markExhausted key =
  modifyTableEntry key $ \entry ->
    entry {entryExhausted = True}

modifyTableEntry :: TableKey -> (TableEntry -> TableEntry) -> TabledM ()
modifyTableEntry key f =
  modify $ \st ->
    st
      { searchTableEntries =
          Map.insertWith
            (\new _old -> new)
            key
            (f (fromMaybe emptyTableEntry (Map.lookup key (searchTableEntries st))))
            (searchTableEntries st)
      }

emptyTableEntry :: TableEntry
emptyTableEntry = TableEntry [] [] False

instantiateAnswer :: ResolutionAnswer -> Pred -> Maybe (Subst, [Pred])
instantiateAnswer answer goal =
  case match (answerGoal answer) goal of
    Left _ -> Nothing
    Right theta ->
      Just (instantiateSubst theta (answerSubst answer), apply theta (answerResidual answer))

instantiateSubst :: Subst -> Subst -> Subst
instantiateSubst theta (Subst xs) =
  Subst (mapMaybe instantiateOne xs)
  where
    instantiateOne (v, t) =
      case apply theta (Meta v) of
        Meta v' -> Just (v', apply theta t)
        _ -> Nothing

tableKey :: Pred -> TableKey
tableKey = TableKey . canonicalPred

type CanonicalState = (Map.Map MetaTv Ty, Map.Map Tyvar Ty, Int)

emptyCanonicalState :: CanonicalState
emptyCanonicalState = (Map.empty, Map.empty, 0)

canonicalPreds :: [Pred] -> [Pred]
canonicalPreds ps =
  evalState (mapM canonicalPredM ps) emptyCanonicalState

canonicalPred :: Pred -> Pred
canonicalPred p =
  case canonicalPreds [p] of
    [p'] -> p'
    _ -> p

canonicalPredM :: Pred -> State CanonicalState Pred
canonicalPredM (InCls n t ts) =
  InCls n <$> canonicalTyM t <*> mapM canonicalTyM ts
canonicalPredM (t1 :~: t2) =
  (:~:) <$> canonicalTyM t1 <*> canonicalTyM t2

canonicalTyM :: Ty -> State CanonicalState Ty
canonicalTyM (TyCon n ts) =
  TyCon n <$> mapM canonicalTyM ts
canonicalTyM (Meta v) =
  canonicalMetaM v
canonicalTyM (TyVar v) =
  canonicalTyvarM v

canonicalMetaM :: MetaTv -> State CanonicalState Ty
canonicalMetaM v = do
  (metas, tyvars, next) <- get
  case Map.lookup v metas of
    Just t -> pure t
    Nothing -> do
      let t = TyVar (TVar (Name ("$tc" ++ show next)))
      put (Map.insert v t metas, tyvars, next + 1)
      pure t

canonicalTyvarM :: Tyvar -> State CanonicalState Ty
canonicalTyvarM v = do
  (metas, tyvars, next) <- get
  case Map.lookup v tyvars of
    Just t -> pure t
    Nothing -> do
      let t = TyVar (TVar (Name ("$tv" ++ show next)))
      put (metas, Map.insert v t tyvars, next + 1)
      pure t

legacyToHnfs :: Int -> [Pred] -> TcM [Pred]
legacyToHnfs depth ps =
  do
    info [">> Solving:", pretty ps]
    ps' <- elimEqualities ps
    ps'' <- withCurrentSubst ps'
    -- here we reduce using instances until we
    -- reach a fixpoint or exaust the maximum depth
    ps0 <- legacyToHnfs' depth ps''
    if ps0 == ps''
      then pure ps0
      else legacyToHnfs (depth - 1) ps0

legacyToHnfs' :: Int -> [Pred] -> TcM [Pred]
legacyToHnfs' _ [] = pure []
legacyToHnfs' 0 ps =
  notEnoughFuel ps
legacyToHnfs' depth (p : ps) =
  do
    let depth' = depth - 1
    rs1 <- legacyToHnf depth' p
    ps' <- withCurrentSubst ps
    rs2 <- legacyToHnfs' depth' ps'
    pure (rs1 ++ rs2)

legacyToHnf :: Int -> Pred -> TcM [Pred]
legacyToHnf depth p@(InCls c _ _)
  | isHnf p =
      do
        info [">>> Solving:", pretty p, " (HNF)"]
        insts <- askInstEnv c
        case byInstM insts p of
          Just (ps', s, i) -> do
            info [">>> Found instance for:", pretty p, "\n>>> Instance:", pretty i, "\n>>> Subst:", pretty s]
            _ <- extSubst s
            ps0 <- withCurrentSubst ps'
            legacyToHnfs (depth - 1) ps0
          Nothing -> do
            insts' <- mapM fromANF insts
            info [">>> No matching instance for:", pretty p, " trying a default instance.Defined instances:\n", unlines (map pretty insts')]
            denv <- getDefaultInstEnv
            case proveDefaulting denv insts p of
              Nothing -> do
                info [">>>> No default instance found for:", pretty p]
                pure [p]
              Just (_, s) -> do
                info [">>>> Default instance for:", pretty p, " found! (Solved), \n>>> Subst: ", pretty s]
                _ <- extSubst s
                pure []
  | depth <= 0 = notEnoughFuel [p]
  | otherwise =
      do
        info [">> Trying to solve:", pretty p]
        insts <- askInstEnv c
        case byInstM insts p of
          Nothing -> do
            insts' <- mapM fromANF insts
            info [">>> No matching instance for:", pretty p, " trying a default instance.Defined instances:\n", unlines (map pretty insts')]
            denv <- getDefaultInstEnv
            -- does c have a default instance?
            case proveDefaulting denv insts p of
              Nothing -> do
                info [">>>> No default instance found for:", pretty p]
                pure [p]
              Just (_, s) -> do
                info [">>>> Default instance for:", pretty p, " found! (Solved), \n>>> Subst: ", pretty s]
                -- default instances should not have any additional contraints.
                _ <- extSubst s
                pure []
          Just (ps', s, i) -> do
            info [">>> Found instance for:", pretty p, "\n>>> Instance:", pretty i, "\n>>> Subst:", pretty s]
            _ <- extSubst s
            ps0 <- withCurrentSubst ps'
            legacyToHnfs (depth - 1) ps0
legacyToHnf _ (t1 :~: t2) =
  do
    info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved)"]
    s <- unify t1 t2
    info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved: ", pretty s, ")"]
    pure []

-- checking for default instance

proveDefaulting :: InstTable -> [Inst] -> Pred -> Maybe ([Pred], Subst)
proveDefaulting denv ienv (InCls cname t ts)
  -- no instance head unify with current predicate
  | all isNothing [tryInst it | it <- ienv] =
      do
        case Map.lookup cname denv of
          Just [ps :=> InCls _ t' ts'] ->
            case match t' t of
              Left _ -> Nothing
              Right u ->
                case mgu ts ts' of
                  Left _ -> Nothing
                  Right u' ->
                    let s = u' <> u
                     in pure (apply s ps, s)
          _ -> Nothing
  -- some instance can unify with current predicate
  | otherwise = Nothing
  where
    -- checking if a predicate can unify with an instance head.
    -- we just consider the instance head.
    tryInst :: Qual Pred -> Maybe Inst
    tryInst i@(_ :=> InCls _ t' ts') =
      case mgu (t' : ts') (t : ts) of
        Left _ -> Nothing
        Right _ -> Just i
    tryInst i = error ("Internal error: tryInst used on an unsupported constraint: " ++ pretty i)
proveDefaulting _ _ p = error ("Internal error: proveDefaulting used on an unsupported constraint: " ++ pretty p)

byInstM :: [Inst] -> Pred -> Maybe ([Pred], Subst, Inst)
byInstM ienv p@(InCls _ _ _) =
  msum [byInst it p | it <- ienv]
byInstM _ p = error ("Internal error: byInstM used on an unsupported constraint" ++ pretty p)

byInst :: Inst -> Pred -> Maybe ([Pred], Subst, Inst)
byInst i@(ps :=> InCls _ t' ts') (InCls _ t ts) =
  -- matching using instance main type
  case match t' t of
    Left _ -> Nothing
    Right u ->
      -- unifying weak type arguments
      case mgu ts ts' of
        Left _ -> Nothing
        Right u' ->
          let s = u' <> u
           in Just (apply s ps, s, i)
byInst c p = error ("Internal error: byInst used on unsupported constraints: " ++ pretty c ++ " / " ++ pretty p)

byInstsM :: [Inst] -> Pred -> [([Pred], Subst, Inst)]
byInstsM ienv p@(InCls _ _ _) =
  mapMaybe (`byInst` p) ienv
byInstsM _ p = error ("Internal error: byInstsM used on an unsupported constraint" ++ pretty p)

bySuperM :: ClassTable -> Pred -> [Pred]
bySuperM ctable p@(InCls c _ _) =
  case Map.lookup c ctable of
    Nothing -> [p]
    Just cinfo ->
      case match (classpred cinfo) p of
        Left _ -> [p]
        Right u -> p : concatMap (bySuperM ctable) (apply u $ supers cinfo)
bySuperM _ _ = []

isHnf :: Pred -> Bool
isHnf (InCls _ t _) = hnf t
isHnf _ = False

hnf :: Ty -> Bool
hnf (TyCon _ _) = False
hnf _ = True

elimEqualities :: [Pred] -> TcM [Pred]
elimEqualities ps0 = go [] ps0
  where
    go rs [] = return rs
    go rs ((t :~: u) : ps) = do
      phi <- mgu t u
      _ <- extSubst phi
      ps' <- withCurrentSubst ps
      rs' <- withCurrentSubst rs
      go rs' ps'
    go rs (p : ps) = go (p : rs) ps

-- entailment

legacyEntail :: ClassTable -> InstTable -> [Pred] -> Pred -> Bool
legacyEntail ctable itable qs p@(InCls n _ _) =
  case Map.lookup n itable of
    Nothing -> p `elem` qs
    Just its ->
      any (p `elem`) (map (bySuperM ctable) qs)
        || p `elem` qs
        || case byInstM its p of
          Nothing -> False
          Just (ps', s, _) ->
            let ps1 = apply s ps'
                qs1 = apply s qs
             in all (legacyEntail ctable itable qs1) ps1
legacyEntail _ _ qs (t1 :~: t2) = t1 == t2 || (t1 :~: t2) `elem` qs

type EntailKey = ([Pred], Pred)

type EntailTable = Map.Map EntailKey Bool

type ActiveEntails = Set.Set EntailKey

tabledEntail :: ClassTable -> InstTable -> [Pred] -> Pred -> Bool
tabledEntail ctable itable qs p =
  fst (tabledEntailWith ctable itable Set.empty Map.empty qs p)

tabledEntailWith :: ClassTable -> InstTable -> ActiveEntails -> EntailTable -> [Pred] -> Pred -> (Bool, EntailTable)
tabledEntailWith ctable itable active table qs p =
  case Map.lookup key table of
    Just r -> (r, table)
    Nothing
      | key `Set.member` active ->
          (False, Map.insert key False table)
      | otherwise ->
          let (r, table') = tabledEntailStep ctable itable (Set.insert key active) table qs p
           in (r, Map.insert key r table')
  where
    key = canonicalEntailKey qs p

canonicalEntailKey :: [Pred] -> Pred -> EntailKey
canonicalEntailKey qs p =
  let allPreds = canonicalPreds (qs ++ [p])
   in (take (length qs) allPreds, last allPreds)

tabledEntailStep :: ClassTable -> InstTable -> ActiveEntails -> EntailTable -> [Pred] -> Pred -> (Bool, EntailTable)
tabledEntailStep ctable itable active table qs p@(InCls n _ _)
  | any (p `elem`) (map (bySuperM ctable) qs) || p `elem` qs =
      (True, table)
  | otherwise =
      case Map.lookup n itable of
        Nothing -> (False, table)
        Just its ->
          case byInstM its p of
            Nothing -> (False, table)
            Just (ps', s, _) ->
              let ps1 = apply s ps'
                  qs1 = apply s qs
               in tabledEntailAll ctable itable active table qs1 ps1
tabledEntailStep _ _ _ table qs (t1 :~: t2) =
  (t1 == t2 || (t1 :~: t2) `elem` qs, table)

tabledEntailAll :: ClassTable -> InstTable -> ActiveEntails -> EntailTable -> [Pred] -> [Pred] -> (Bool, EntailTable)
tabledEntailAll _ _ _ table _ [] = (True, table)
tabledEntailAll ctable itable active table qs (p : ps) =
  let (r, table') = tabledEntailWith ctable itable active table qs p
   in if r
        then tabledEntailAll ctable itable active table' qs ps
        else (False, table')

-- error messages

notEnoughFuel :: [Pred] -> TcM a
notEnoughFuel ps =
  tcmError $
    unlines
      [ "Cannot solve:",
        pretty ps,
        "because the solver exceeded the maximum number of iterations."
      ]

undefinedInstance :: Pred -> TcM a
undefinedInstance p@(InCls n _ _) =
  do
    insts <- askInstEnv n
    insts' <- mapM fromANF insts
    tcmError $
      unlines $
        [ "Cannot entail:",
          f (pretty p),
          "currently defined instances:"
        ]
          ++ map (f . pretty) insts'
  where
    f s = "   " ++ s
undefinedInstance p = tcmError $ unwords ["Cannot entail: ", pretty p]

unsolvedError :: [Pred] -> TcM ()
unsolvedError = mapM_ unsolvedPredError

unsolvedPredError :: Pred -> TcM ()
unsolvedPredError p@(InCls n _ _) =
  do
    insts <- askInstEnv n
    insts' <- mapM fromANF insts
    let s = unlines (map pretty insts')
    tcmError $
      unlines
        [ "Cannot entail:",
          pretty p,
          "using defined instances:",
          s
        ]
unsolvedPredError p = tcmError $ unwords ["Cannot entail:", pretty p]
