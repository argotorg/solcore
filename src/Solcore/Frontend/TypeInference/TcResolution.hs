module Solcore.Frontend.TypeInference.TcResolution where

import Control.Monad
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

type ResolutionTable = Map.Map Pred [Pred]

type ActiveGoals = Set.Set Pred

tabledToHnfs :: Int -> [Pred] -> TcM [Pred]
tabledToHnfs depth ps =
  fst <$> tabledToHnfsLoop Set.empty Map.empty depth ps

tabledToHnfsLoop :: ActiveGoals -> ResolutionTable -> Int -> [Pred] -> TcM ([Pred], ResolutionTable)
tabledToHnfsLoop active table depth ps =
  do
    info [">> Solving (tabled):", pretty ps]
    ps' <- elimEqualities ps
    ps'' <- withCurrentSubst ps'
    ps0AndTable <- tabledToHnfsOnce active table depth ps''
    let (ps0, table') = ps0AndTable
    if ps0 == ps''
      then pure ps0AndTable
      else tabledToHnfsLoop active table' (depth - 1) ps0

tabledToHnfsOnce :: ActiveGoals -> ResolutionTable -> Int -> [Pred] -> TcM ([Pred], ResolutionTable)
tabledToHnfsOnce _ table _ [] = pure ([], table)
tabledToHnfsOnce _ _ depth ps
  | depth <= 0 =
      notEnoughFuel ps
tabledToHnfsOnce active table depth (p : ps) =
  do
    let depth' = depth - 1
    (rs1, table1) <- tabledToHnf active table depth' p
    ps' <- withCurrentSubst ps
    (rs2, table2) <- tabledToHnfsOnce active table1 depth' ps'
    pure (rs1 ++ rs2, table2)

tabledToHnf :: ActiveGoals -> ResolutionTable -> Int -> Pred -> TcM ([Pred], ResolutionTable)
tabledToHnf active table depth p0@(InCls c _ _) =
  do
    p <- withCurrentSubst p0
    case Map.lookup p table of
      Just rs -> do
        info [">>> Tabled resolution cache hit for:", pretty p]
        rs' <- withCurrentSubst rs
        pure (rs', table)
      Nothing
        | p `Set.member` active -> do
            info [">>> Tabled resolution cycle for:", pretty p, " (deferred)"]
            pure ([p], table)
        | isHnf p ->
            do
              info [">>> Solving (tabled):", pretty p, " (HNF)"]
              tabledByInst (Set.insert p active) table depth p c
        | depth <= 0 -> notEnoughFuel [p]
        | otherwise ->
            do
              info [">> Trying to solve (tabled):", pretty p]
              tabledByInst (Set.insert p active) table depth p c
tabledToHnf _ table _ (t1 :~: t2) =
  do
    info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved)"]
    s <- unify t1 t2
    info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved: ", pretty s, ")"]
    pure ([], table)

tabledByInst :: ActiveGoals -> ResolutionTable -> Int -> Pred -> Name -> TcM ([Pred], ResolutionTable)
tabledByInst active table depth p c =
  do
    insts <- askInstEnv c
    case byInstM insts p of
      Just (ps', s, i) -> do
        info [">>> Found tabled instance for:", pretty p, "\n>>> Instance:", pretty i, "\n>>> Subst:", pretty s]
        _ <- extSubst s
        ps0 <- withCurrentSubst ps'
        (rs, table') <- tabledToHnfsLoop active table (depth - 1) ps0
        rs' <- withCurrentSubst rs
        pure (rs', Map.insert p rs' table')
      Nothing -> do
        insts' <- mapM fromANF insts
        info [">>> No matching tabled instance for:", pretty p, " trying a default instance.Defined instances:\n", unlines (map pretty insts')]
        denv <- getDefaultInstEnv
        case proveDefaulting denv insts p of
          Nothing -> do
            info [">>>> No default instance found for:", pretty p]
            pure ([p], Map.insert p [p] table)
          Just (_, s) -> do
            info [">>>> Default instance for:", pretty p, " found! (Solved), \n>>> Subst: ", pretty s]
            _ <- extSubst s
            pure ([], Map.insert p [] table)

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
byInstM ienv (InCls _ t ts) =
  msum [tryInst it | it <- ienv]
  where
    tryInst :: Qual Pred -> Maybe ([Pred], Subst, Inst)
    tryInst i@(ps :=> InCls _ t' ts') =
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
    tryInst c = error ("Internal error: tryInst used on an unsupported constraint: " ++ pretty c)
byInstM _ p = error ("Internal error: byInstM used on an unsupported constraint" ++ pretty p)

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
    key = (qs, p)

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
