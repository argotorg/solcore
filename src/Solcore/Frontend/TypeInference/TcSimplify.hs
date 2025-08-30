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
      info ["< Reduced wanted constraints:", pretty ps2]
      -- simplify constraints using given constraints
      qs2 <- withCurrentSubst qs'
      info [">!! Simplifying ", pretty ps2, " using ", pretty qs2]
      rs <- simplify qs2 ps2
      info ["<!! Final simplified constraints:", pretty rs]
      unsolved <- checkEntails qs2 rs
      info ["<<!! Unsolved:", pretty unsolved]
      unless (null unsolved) $ do
        tcmError $ unlines $ [ "Cannot satisfy:"
                             , pretty unsolved
                             , "using currently defined instances"
                             ]
      pure rs

checkEntails :: [Pred] -> [Pred] -> TcM [Pred]
checkEntails qs rs
  = do
      ctable <- getClassEnv
      itable <- getInstEnv
      info [">>! Trying to check the entailment of:", pretty rs, " from:", pretty qs]
      let
          qs' = nub $ concatMap (bySuperM ctable) qs
          unsolved q = not (isInvoke q) && not (entail ctable itable qs' q)
          -- compiler generated instances can introduce invokable constraints
          -- no present in the called function. Since type inference can produce
          -- such constraints, we do not consider them here.
          isInvoke (InCls n _ _) = n == (Name "invokable")
          isInvoke _ = False
      info [">>! Simplified given constraints:", pretty qs']
      pure $ filter unsolved rs

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
      info [">> Reducing ", pretty ps, " using instances"]
      ps' <- elimEqualities ps
      info [">> After eliminating equalities:", pretty ps']
      ps'' <- withCurrentSubst ps'
      toHnfs' depth ps''

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
        insts <- askInstEnv c
        case byInstM insts p of
          Nothing -> do
            info [">>> No matching instance for:", pretty p, " trying a default instance."]
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
          Just (ps' , s, i) -> do
            info [">>> Found instance for:", pretty p, "\n>>>Instance:", pretty i,"\n>>>Subst:", pretty s]
            _ <- extSubst s
            toHnfs (depth - 1) ps'
toHnf _ (t1 :~: t2)
  = do
      info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved)"]
      s <- unify t1 t2
      info [">>> Unify ", pretty t1, " with ", pretty t2, " (Solved: ", pretty s, ")"]
      pure []

-- checking for default instance

proveDefaulting :: InstTable -> [Inst] -> Pred -> Maybe ([Pred], Subst)
proveDefaulting denv ienv p@(InCls i t ts)
  -- no instance head unify with current predicate
  | all isNothing [tryInst it | it <- ienv]
    = do
        case Map.lookup i denv of
          Just [(ps :=> h@(InCls _ t' ts'))] ->
            case match t' t of
              Left _ -> Nothing
              Right u ->
                case mgu ts ts' of
                  Left _ -> Nothing
                  Right u' ->
                    let vs = mv h
                        s = u' <> u
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

byInstM :: [Inst] -> Pred -> Maybe ([Pred], Subst, Inst)
byInstM ienv p@(InCls i t ts)
  = msum [tryInst it | it <- ienv]
    where
      tryInst :: Qual Pred -> Maybe ([Pred], Subst, Inst)
      tryInst c@(ps :=> h@(InCls _ t' ts')) =
        -- matching using instance main type
        case match t' t of
          Left _ -> Nothing
          Right u ->
            -- unifying weak type arguments
            case mgu ts ts' of
              Left _ -> Nothing
              Right u' ->
                let tvs = mv h
                    s = u' <> u
                in  Just (apply s ps, s, c)

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

fromANF (ps :=> p)
  = do
      let eqs = [ (t,t') | (t :~: t') <- ps]
      s <- solve eqs mempty
      pure $ apply s ([] :=> p)
