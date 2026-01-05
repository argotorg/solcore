module Solcore.Frontend.TypeInference.TcSimplify where

import Control.Monad
import Data.Either (isRight)
import Data.List
import Data.Map qualified as Map
import Data.Maybe

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad hiding(insts)
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify



-- context reduction

reduce :: [Pred] -> -- given constraints
          [Pred] -> -- wanted constraints
          TcM [Pred]
reduce qs0 ps0
  = do
      depth <- askMaxRecursionDepth
      ps <- withCurrentSubst ps0
      qs <- withCurrentSubst qs0
      -- reduce wanted constraints
      info ["> Reducing wanted constraints:", pretty ps, " using ", pretty qs]
      ctable <- getClassEnv
      itable <- getInstEnv
      let
        qs' = concatMap (bySuperM ctable) qs
        ps' = filter (not . entail ctable itable qs') ps
      info ["> After entailment:", pretty ps', " - ", pretty qs']
      ps1 <- toHnfs depth ps'
      ps2 <- withCurrentSubst ps1
      info ["< Reduced by instances constraints:", pretty ps2]
      -- simplify constraints using given constraints
      qs2 <- withCurrentSubst qs'
      info [">!! Simplifying ", pretty ps2, " using ", pretty qs2]
      ps3 <- simplify qs2 ps2
      rs <- enforceDependencies ps3
      info ["<!! Final simplified constraints:", pretty rs]
      unsolved <- checkEntails qs2 rs
      info ["<<!! Unsolved:", pretty unsolved]
      unless (null unsolved) (unsolvedError unsolved)
      pure rs

enforceDependencies :: [Pred] -> TcM [Pred]
enforceDependencies ps
  = do
      let
        eqMainTy p p' = predName p == predName p' &&
                        predMain p == predMain p'
        pss = splits eqMainTy ps
      s <- foldr (<>) mempty <$> mapM unifyWeakArgs pss
      extSubst s
      pure (nub $ apply s ps)

unifyWeakArgs :: [Pred] -> TcM Subst
unifyWeakArgs []
  = pure mempty
unifyWeakArgs [_]
  = pure mempty
unifyWeakArgs (p1 : p2 : ps)
  = do
      s1 <- unifyWeakArgs (p2 : ps)
      s2 <- unifyTypes (apply s1 $ predParams p1)
                       (apply s1 $ predParams p2)
      pure (s2 <> s1)

splits :: (a -> a -> Bool) -> [a] -> [[a]]
splits _ [] = []
splits eq (x:xs) = (x : filter (eq x) xs) : splits eq (filter (not . eq x) xs)

checkEntails :: [Pred] -> [Pred] -> TcM [Pred]
checkEntails qs rs
  = do
      ctable <- getClassEnv
      itable <- getInstEnv
      info [">>! Trying to check the entailment of:", pretty rs, " from:", pretty qs]
      let
          qs' = nub $ concatMap (bySuperM ctable) qs
          unsolved q = not (isHnf q) && not (isInvoke q) && not (entail ctable itable qs' q)
          -- compiler generated instances can introduce invokable constraints
          -- not present in the called function. Since type inference can produce
          -- such constraints, we do not consider them here.
      info [">>! Simplified given constraints:", pretty qs']
      pure $ filter unsolved rs

isInvoke :: Pred -> Bool
isInvoke (InCls n _ _) = n == (Name "invokable")
isInvoke _ = False

simplify :: [Pred] -> [Pred] -> TcM [Pred]
simplify qs ps
  = do
      ctable <- getClassEnv
      itable <- getInstEnv
      let qs' = concatMap (bySuperM ctable) qs
      simplify' ctable itable qs' ps
  where
    simplify' :: ClassTable -> InstTable -> [Pred] -> [Pred] -> TcM [Pred]
    simplify' _ _ rs [] = pure rs
    simplify' ct it rs (p' : ps')
      | entail ct it (rs `union` ps') p'
        = do
            info [">> Entailing: ", pretty p', " by ", pretty (rs `union` ps')]
            simplify' ct it rs ps'
      | otherwise
        = do
            info [">>", pretty p', " cannot be entailed by:", pretty (rs `union` ps')]
            simplify' ct it (p' : rs) ps'

-- converting to head normal forms

toHnfs :: Int -> [Pred] -> TcM [Pred]
toHnfs depth ps
  = do
      info [">> Solving:", pretty ps]
      ps' <- elimEqualities ps
      ps'' <- withCurrentSubst ps'
      -- here we reduce using instances until we
      -- reach a fixpoint or exaust the maximum depth
      ps0 <- toHnfs' depth ps''
      if ps0 == ps'' then pure ps0
        else toHnfs (depth - 1) ps0

toHnfs' :: Int -> [Pred] -> TcM [Pred]
toHnfs' _ [] = pure []
toHnfs' 0 ps
  = notEnoughFuel ps
toHnfs' depth (p : ps)
  = do
      let depth' = depth - 1
      rs1 <- toHnf depth' p
      ps' <- withCurrentSubst ps
      rs2 <- toHnfs' depth' ps'
      pure (rs1 ++ rs2)

toHnf :: Int -> Pred -> TcM [Pred]
toHnf depth p@(InCls c _ _)
  | isHnf p
    = do
        info [">>> Solving:", pretty p, " (HNF)"]
        pure [p]
  | depth <= 0 = notEnoughFuel [p]
  | otherwise
    = do
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
                info [">>>> Default instance for:", pretty p, "found! (Solved)"]
                -- default instances should not have any additional contraints.
                _ <- extSubst s
                pure []
          Just (ps', s, i) -> do
            info [">>> Found instance for:", pretty p, "\n>>> Instance:", pretty i,"\n>>> Subst:", pretty s]
            _ <- extSubst s
            ps0 <- withCurrentSubst ps'
            toHnfs (depth - 1) ps0
toHnf _ (t1 :~: t2)
  = do
      info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved)"]
      s <- unify t1 t2
      info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved: ", pretty s, ")"]
      pure []

-- checking for default instance

proveDefaulting :: InstTable -> [Inst] -> Pred -> Maybe ([Pred], Subst)
proveDefaulting denv ienv p@(InCls cname t ts)
  -- no instance head unify with current predicate
  | all isNothing [tryInst it | it <- ienv]
    = do
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
  tryInst i@(_ :=> h@(InCls _ t' ts')) =
    case mgu (t' : ts') (t : ts) of
      Left _ -> Nothing
      Right _ -> Just i
  tryInst i = error ("Internal error: tryInst used on an unsupported constraint: " ++ pretty i)
proveDefaulting _ _ p  = error ("Internal error: proveDefaulting used on an unsupported constraint: " ++ pretty p)

byInstM :: [Inst] -> Pred -> Maybe ([Pred], Subst, Inst)
byInstM ienv (InCls _ t ts)
  = msum [tryInst it | it <- ienv]
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
                in  Just (apply s ps, s, i)
      tryInst c = error ("Internal error: tryInst used on an unsupported constraint: " ++ pretty c)
byInstM _ p  = error ("Internal error: byInstM used on an unsupported constraint" ++ pretty p)

bySuperM :: ClassTable -> Pred -> [Pred]
bySuperM ctable p@(InCls c _ _)
  = case Map.lookup c ctable of
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
elimEqualities ps0 = go [] ps0 where
    go rs [] = return rs
    go rs ((t :~: u) : ps) = do
      phi <- mgu t u
      _ <- extSubst phi
      ps' <- withCurrentSubst ps
      rs' <- withCurrentSubst rs
      go rs' ps'
    go rs (p:ps) = go (p:rs) ps

-- entailment

entail :: ClassTable -> InstTable -> [Pred] -> Pred -> Bool
entail ctable itable qs p@(InCls n _ _)
  = case Map.lookup n itable of
      Nothing -> p `elem` qs
      Just its -> any (p `elem`) (map (bySuperM ctable) qs) ||
                  p `elem` qs ||
                  case byInstM its p of
                    Nothing -> False
                    Just (ps', s, _) ->
                      let ps1 = apply s ps'
                          qs1 = apply s qs
                      in all (entail ctable itable qs1) ps1
entail _ _ _ (_ :~: _) = False

-- error messages

notEnoughFuel :: [Pred] -> TcM a
notEnoughFuel ps
  = tcmError $ unlines ["Cannot solve:"
                       , pretty ps
                       , "because the solver exceeded the maximum number of iterations."]

undefinedInstance :: Pred -> TcM a
undefinedInstance p@(InCls n _ _)
  = do
      insts <- askInstEnv n
      insts' <- mapM fromANF insts
      tcmError $ unlines $ ["Cannot entail:"
                           , f (pretty p)
                           , "currently defined instances:"
                           ] ++ map (f . pretty) insts'
    where
      f s = "   " ++ s
undefinedInstance p = tcmError $ unwords ["Cannot entail: ", pretty p]

unsolvedError :: [Pred] -> TcM ()
unsolvedError = mapM_ unsolvedPredError

unsolvedPredError :: Pred -> TcM ()
unsolvedPredError p@(InCls n _ _)
  = do
      insts <- askInstEnv n
      insts' <- mapM fromANF insts
      let s = unlines (map pretty insts')
      tcmError $ unlines [ "Cannot entail:"
                         , pretty p
                         , "using defined instances:", s]
