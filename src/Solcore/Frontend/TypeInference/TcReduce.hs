module Solcore.Frontend.TypeInference.TcReduce where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.Either (isRight)
import Data.List
import Data.Map qualified as Map
import Data.Maybe

import Solcore.Frontend.Parser.SolverInputParser hiding (insts)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify

import Solcore.Pipeline.Options

splitContext :: [Pred] -> [Pred] -> [MetaTv] -> TcM ([Pred], [Pred])
splitContext ps qs fs =
  do
    unless (null ps) $ infoDocs[text ">> Starting the reduction of:", ppr ps, text "using", ppr qs]
    ps' <- reduce ps qs
    let (ds, rs) = partition (all (`elem` fs) . mv) ps'
    info [">> Deferred constraints:", pretty ds]
    info [">> Retained constraints:", pretty rs]
    when (groundPred ds) $
      tcmError $ unwords ["No instance found for:", unlines $ map pretty ds]
    pure (ds, rs)

-- main context reduction function

reduce :: [Pred] -> [Pred] -> TcM [Pred]
reduce ps0 qs =
  do
    n <- askMaxRecursionDepth
    ps' <- reduce' n ps0 qs
    simplify ps'

reduce' :: Int -> [Pred] -> [Pred] -> TcM [Pred]
reduce' n ps qs
  = do
      ps' <- reduceI n ps qs
      reduceI n ps' qs

reduceI :: Int -> [Pred] -> [Pred] -> TcM [Pred]
reduceI n ps0 qs
  | n <= 0 =
      tcmError $
        unwords
          [ "Cannot reduce"
          , pretty ps0
          , "since the solver exceeded the max number of iterations"
          ]
  | otherwise =
      do
        ps <- withCurrentSubst ps0
        preds <- concat <$> mapM reduceBySuper ps
        unless (null preds) $ info ["> reduce context ", pretty preds]
        ps1 <- reduceByInst n preds qs
        unless (null ps1) $ info ["< reduced context ", pretty (nub ps1)]
        pure (nub ps1)

-- simplify by entailment

simplify :: [Pred] -> TcM [Pred]
simplify = loop []
 where
  loop rs [] = pure rs
  loop rs (p : ps) = do
    c <- entails (rs ++ ps) p
    if c
      then loop rs ps
      else loop (p : rs) ps

-- reducing by using instance information

reduceByInst :: Int -> [Pred] -> [Pred] -> TcM [Pred]
reduceByInst n ps qs =
  (nub . concat) <$> mapM (\ p -> do
      ps' <- reduceByInst' n qs p
      info [">> Solved: ", pretty p, ", remaining: ", prettys ps']
      pure ps') ps

reduceByInst' :: Int -> [Pred] -> Pred -> TcM [Pred]
reduceByInst' n qs p@(InCls c _ _)
  | n <= 0 =
      tcmError $
        unwords
          [ "Cannot reduce"
          , pretty p
          , "since the solver exceeded the max number of iterations."
          ]
  | inHnf p = do
    if checkEntails qs p then pure []
    else do
      info [">> Solving (HNF):", pretty p]
      pure [p]
  | otherwise =
      do
        ce <- getInstEnv
        pp <- withCurrentSubst p
        info [">> Trying to solve ", pretty pp]
        r <- findInst pp
        case r of
          Nothing -> do
            info ["   instance for ", pretty pp, " not found!"]
            de <- getDefaultInstEnv
            if proveDefaulting ce pp
              then do
                case selectDefaultInst de pp of
                  Nothing ->
                    if checkEntails qs pp then do
                        pure []
                      else pure [pp]
                  Just (ps', s, h) -> do
                    extSubst s
                    withCurrentSubst ps'
              else do
                if checkEntails qs pp then do
                    pure []
                  else pure [pp]
          Just (preds, subst', instd) -> do
            info ["   instance for ", pretty pp, " found: ", pretty instd, "@", pretty subst']
            extSubst subst'
            ps' <- reduceByInst (n - 1) preds qs
            pure ps'
reduceByInst' n _ (t1 :~: t2) =
  do
    unify t1 t2
    pure []

groundPred :: [Pred] -> Bool
groundPred p = (not $ null p) && null (mv p)

checkEntails :: [Pred] -> Pred -> Bool
checkEntails qs p = any (\ q -> isRight $ mgu q p) qs

proveDefaulting :: InstTable -> Pred -> Bool
proveDefaulting ce p@(InCls i t as) =
  isNothing (msum [tryInst it | it <- insts ce i])
 where
  insts m n = maybe [] id (Map.lookup n m)
  tryInst :: Qual Pred -> Maybe Inst
  tryInst i@(_ :=> h) =
    case mgu h p of
      Left _ -> Nothing
      Right _ -> Just i

selectDefaultInst :: Table Inst -> Pred -> Maybe ([Pred], Subst, Inst)
selectDefaultInst de p@(InCls i t as) =
  case Map.lookup i de of
    Nothing -> Nothing
    Just c@(ps :=> (InCls _ t' ts')) ->
      case match t' t of
        Left _ -> Nothing
        Right u ->
          case unifyTypes as ts' of
            Left _ -> Nothing
            Right u' ->
              let u1 = u' <> u
               in Just (map (apply u1) ps, u1, c)
selectDefaultInst _ _ = Nothing

selectInst :: InstTable -> Pred -> Maybe ([Pred], Subst, Inst)
selectInst ce p@(InCls i t as) =
  msum [tryInst it | it <- searchInsts ce i]
 where
  searchInsts m n = maybe [] id (Map.lookup n m)
  tryInst :: Qual Pred -> Maybe ([Pred], Subst, Inst)
  tryInst c@(ps :=> (InCls _ t' ts)) =
    case match t' t of
      Left _ -> Nothing
      Right u -> case unifyTypes as ts of
        Left _ -> Nothing
        Right u' ->
          let
            u1 = u' <> u
           in
            Just (map (apply u1) ps, u1, c)
selectInst _ _ = Nothing

askInstancesFor :: Pred -> TcM [Inst]
askInstancesFor (InCls n _ _)
  = Map.findWithDefault [] n <$> getInstEnv

findInst :: Pred -> TcM (Maybe ([Pred], Subst, Inst))
findInst p
  = do
      insts <- askInstancesFor p
      msum <$> mapM (solvePred p) insts

solvePred :: Pred -> Qual Pred -> TcM (Maybe ([Pred], Subst, Inst))
solvePred p@(InCls _ t ts) ins@(ps :=> h@(InCls _ t' ts'))
  = do
      infoDocs [text "> Trying to solve:", ppr p, text "using", ppr ins]
      infoDocs [text ">> Trying to match:", ppr t', "with", ppr t]
      case match t' t of
        Left _ -> do
          -- info ["!>> Predicate ", pretty p, " cannot be solved by ", pretty h ,", since main args do not match ", pretty t', " and ", pretty t]
          pure Nothing
        Right u ->
          case mgu (apply u ts) (apply u ts') of
            Left _ -> do
              -- info ["!>> Predicate ", pretty p, " cannot be solved by ", pretty h, ", since weak args do not unify"]
              pure Nothing
            Right u' -> do
              let u1 = u' <> u
              -- info ["!>> Predicate ", pretty p, " matches instance ", pretty h]
              pure $ Just (apply u1 ps, u1, ins)


-- reducing by super class info

reduceBySuper :: Pred -> TcM [Pred]
reduceBySuper p =
  do
    n <- askMaxRecursionDepth
    reduceBySuper' n p

reduceBySuper' :: Int -> Pred -> TcM [Pred]
reduceBySuper' _ (_ :~: _) = pure []
reduceBySuper' n p@(InCls c _ _)
  | n <= 0 =
      tcmError $
        unwords
          [ "Cannot reduce:"
          , pretty p
          , "since the solver exceeded the max number of iterations."
          ]
  | otherwise = do
      cinfo <- askClassInfo c
      info ["> Reducing by super class:", pretty p]
      sm <- match (classpred cinfo) p
      info [">> Reduced by super class:", pretty (apply sm (supers cinfo))]
      extSubst sm
      ps' <- concat <$> mapM (reduceBySuper' (n - 1)) (apply sm (supers cinfo))
      pure (p : ps')

-- entailment

entails :: [Pred] -> Pred -> TcM Bool
entails qs p =
  do
    n <- askMaxRecursionDepth
    entails' n qs p

entails' :: Int -> [Pred] -> Pred -> TcM Bool
entails' n qs p
  | n <= 0 =
      tcmError $
        unwords
          [ "Cannot reduce:"
          , pretty p
          , "since the solver exceeded the max number of iterations."
          ]
  | otherwise = do
      ce <- getInstEnv
      info [">>> Testing entailment of:", pretty p, "using:", if null qs then "<empty>" else pretty qs]
      qs' <- mapM reduceBySuper qs
      info [">>> Reduced by superclass:", if null qs' then "<empty>" else pretty (concat qs')]
      r <- case selectInst ce p of
        Nothing -> pure False
        Just (ps, s, h) ->
          and <$> mapM (entails' (n - 1) ps) (apply s ps)
      pure $ (any (p `elem`) qs') || r

-- hnf for predicates

inHnf :: Pred -> Bool
inHnf (InCls _ t _) = hnf t
inHnf _ = False

-- head normal form for types

hnf :: Ty -> Bool
hnf (Meta _) = True
hnf (TyVar _) = True
hnf _ = False

-- function matches
-- We assume that we do not support
-- overlapping instances.

matches :: Pred -> TcM (Maybe (Subst, [Pred]))
matches p@(InCls c t ts) =
  do
    insts <- askInstEnv c
    ms <- catMaybes <$> mapM (genM t) insts
    case ms of
      [] -> pure Nothing
      [x] -> pure (Just x)
      ins ->
        throwError $
          unwords
            [ "Constraint"
            , pretty p
            , "has more than one matching instance."
            ]
matches p = tcmError $ "Invalid constraint:" ++ pretty p

genM :: Ty -> Inst -> TcM (Maybe (Subst, [Pred]))
genM t k@(ps :=> h@(InCls _ t' _)) =
  do
    r <- defaultM (match t t')
    case r of
      Just s -> pure $ Just (s, apply s ps)
      Nothing -> pure Nothing
