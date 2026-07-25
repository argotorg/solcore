module Solcore.Frontend.TypeInference.TcSimplify where

import Control.Monad
import Data.List
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad hiding (insts)
import Solcore.Frontend.TypeInference.TcResolution
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify

-- context reduction

reduce ::
  [Pred] -> -- given constraints
  [Pred] -> -- wanted constraints
  TcM [Pred]
reduce qs0 ps0 =
  do
    depth <- askMaxRecursionDepth
    ps <- withCurrentSubst ps0
    qs <- withCurrentSubst qs0
    -- reduce wanted constraints
    info ["> Reducing wanted constraints:", pretty ps, " using ", pretty qs]
    ctable <- getClassEnv
    itable <- getInstEnv
    qs' <- superPredsM ctable qs
    ps' <- filterM (\p -> not <$> entailM ctable itable qs' p) ps
    info ["> After entailment:", pretty ps', " - ", pretty qs']
    -- Before falling through to instance lookup, try to discharge HNF wanteds
    -- by unifying their weak args against a matching given predicate (same class, same main type).
    psz <- solveByGivens qs' ps'
    ps'' <- withCurrentSubst psz
    ps1 <- toHnfs depth ps''
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
enforceDependencies ps =
  do
    let eqMainTy p p' =
          predName p == predName p'
            && predMain p == predMain p'
        pss = splits eqMainTy ps
    s <- foldr (<>) mempty <$> mapM unifyWeakArgs pss
    _ <- extSubst s
    pure (nub $ apply s ps)

unifyWeakArgs :: [Pred] -> TcM Subst
unifyWeakArgs [] =
  pure mempty
unifyWeakArgs [_] =
  pure mempty
unifyWeakArgs (p1 : p2 : ps) =
  do
    s1 <- unifyWeakArgs (p2 : ps)
    s2 <-
      unifyTypes
        (apply s1 $ predParams p1)
        (apply s1 $ predParams p2)
    pure (s2 <> s1)

splits :: (a -> a -> Bool) -> [a] -> [[a]]
splits _ [] = []
splits eq (x : xs) = (x : filter (eq x) xs) : splits eq (filter (not . eq x) xs)

checkEntails :: [Pred] -> [Pred] -> TcM [Pred]
checkEntails qs rs =
  do
    ctable <- getClassEnv
    itable <- getInstEnv
    noDesugarCalls <- getNoDesugarCalls
    info [">>! Trying to check the entailment of:", pretty rs, " from:", pretty qs]
    qs' <- nub <$> superPredsM ctable qs
    let skip q = isInvoke q || (noDesugarCalls && (isInt q || isStr q))
    -- compiler generated instances can introduce invokable constraints
    -- not present in the called function. Since type inference can produce
    -- such constraints, we do not consider them here.
    -- Int and Str constraints are similarly skipped in noDesugarCalls mode
    -- because the Int.fromInteger and Str.fromString calls introduced by
    -- literal desugaring can't be resolved when lambda closure conversion is
    -- skipped.
    info [">>! Simplified given constraints:", pretty qs']
    filterM (unsolved skip ctable itable qs') rs
  where
    unsolved skip ctable itable qs' q = do
      entailed <- entailM ctable itable qs' q
      pure (not (isHnf q) && not (skip q) && not entailed)

isInvoke :: Pred -> Bool
isInvoke (InCls n _ _) = n == (Name "invokable")
isInvoke _ = False

isInt :: Pred -> Bool
isInt (InCls n _ _) = n == Name "Int"
isInt _ = False

isStr :: Pred -> Bool
isStr (InCls n _ _) = n == Name "Str"
isStr _ = False

simplify :: [Pred] -> [Pred] -> TcM [Pred]
simplify qs ps =
  do
    ctable <- getClassEnv
    itable <- getInstEnv
    qs' <- superPredsM ctable qs
    simplify' ctable itable qs' ps
  where
    simplify' :: ClassTable -> InstTable -> [Pred] -> [Pred] -> TcM [Pred]
    simplify' _ _ rs [] = pure rs
    simplify' ct it rs (p' : ps') =
      do
        entailed <- entailM ct it (rs `union` ps') p'
        if entailed
          then do
            info [">> Entailing: ", pretty p', " by ", pretty (rs `union` ps')]
            simplify' ct it rs ps'
          else do
            info [">>", pretty p', " cannot be entailed by:", pretty (rs `union` ps')]
            simplify' ct it (p' : rs) ps'

-- Discharge HNF wanted predicates using given constraints before resorting to
-- instance lookup.  For each wanted predicate whose main type is in HNF (i.e. a
-- Meta or TyVar, not a concrete TyCon), look for a given predicate with the same
-- class name and the same main type, and unify their weak arguments.  This gives
-- the correct binding when the annotation context already determines the relationship
solveByGivens :: [Pred] -> [Pred] -> TcM [Pred]
solveByGivens _ [] = pure []
solveByGivens qs (p : ps) = do
  p' <- withCurrentSubst p
  case tryDischargeByGivens qs p' of
    Just s -> do
      _ <- extSubst s
      ps' <- withCurrentSubst ps
      qs' <- withCurrentSubst qs
      solveByGivens qs' ps'
    Nothing -> do
      rest <- solveByGivens qs ps
      pure (p' : rest)

tryDischargeByGivens :: [Pred] -> Pred -> Maybe Subst
tryDischargeByGivens qs p@(InCls _ _ _) = msum [tryOneGiven q p | q <- qs]
tryDischargeByGivens _ _ = Nothing

tryOneGiven :: Pred -> Pred -> Maybe Subst
tryOneGiven (InCls cn' mainTy' params') (InCls cn mainTy1 params)
  | cn == cn',
    mainTy1 == mainTy' =
      case mgu params params' of
        Left _ -> Nothing
        Right s -> Just s
tryOneGiven _ _ = Nothing
