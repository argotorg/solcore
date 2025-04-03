module Solcore.Frontend.TypeInference.TcReduce where 

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Solcore.Frontend.Syntax
import Solcore.Frontend.Parser.SolverInputParser
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify

import Solcore.Pipeline.Options


splitContext :: [Pred] -> [Tyvar] -> TcM ([Pred], [Pred])
splitContext ps fs 
  = do 
      ps' <- reduce ps
      let (ds, rs) = partition (all (`elem` fs) . fv) ps'
      info [">> Defered constraints:", pretty ds]
      info [">> Retained constraints:", pretty rs]
      pure (ds, ps)
 

-- main context reduction function 

reduce :: [Pred] -> TcM [Pred]
reduce ps0
  = do 
      n <- askMaxRecursionDepth
      ps' <- reduceI n ps0
      simplify ps'

reduceI :: Int -> [Pred] -> TcM [Pred]
reduceI n ps0
  | n <= 0 = tcmError $ unwords [ "Cannot reduce"
                                , pretty ps0
                                , "since the solver exceeded the max number of iterations"
                                ]
  | otherwise 
    = do
        ps <- withCurrentSubst ps0
        preds <- concat <$> mapM reduceBySuper ps
        unless (null preds) $ info ["> reduce context ", pretty preds]
        ps1 <- reduceByInst n preds
        unless (null ps1) $ info ["< reduced context ", pretty (nub ps1)]
        pure (nub ps1)

-- simplify by entailment 

simplify :: [Pred] -> TcM [Pred]
simplify = loop []
  where 
    loop rs [] = pure rs 
    loop rs (p : ps) = do 
      c <- entails (rs ++ ps) p 
      if c then loop rs ps 
        else loop (p : rs) ps

-- reducing by using instance information

reduceByInst :: Int -> [Pred] -> TcM [Pred]
reduceByInst n ps 
  = (nub . concat) <$> mapM (reduceByInst' n) ps 

reduceByInst' :: Int -> Pred -> TcM [Pred]
reduceByInst' n p@(InCls c _ _) 
  | n <= 0 
    = tcmError $ unwords [ "Cannot reduce"
                         , pretty p
                         , "since the solver exceeded the max number of iterations."
                         ]
  | otherwise 
    = do 
        ce <- getInstEnv
        insts <- askInstEnv c
        case selectInst ce p of
          Nothing -> do 
            pure [p]
          Just (preds, subst', instd) -> do 
            extSubst subst' 
            ps' <- reduceByInst (n - 1) preds
            pure ps'
reduceByInst' n (t1 :~: t2) 
  = do 
      unify t1 t2
      pure []

selectInst :: InstTable -> Pred -> Maybe ([Pred], Subst, Inst)
selectInst ce p@(InCls i t as) 
  = msum [tryInst it | it <- insts ce i] 
    where
      insts m n = maybe [] id (Map.lookup n m)
      tryInst :: Qual Pred -> Maybe ([Pred], Subst, Inst)
      tryInst c@(ps :=> (InCls _ t' ts)) =
          case match t' t of
            Left _ -> Nothing
            Right u -> case unifyTypes as ts of 
                          Left _ -> Nothing 
                          Right u' -> 
                            let
                                u1 = u' <> u
                            in Just (map (apply u1) ps, u1, c)
selectInst _ _ = Nothing


-- reducing by super class info 

reduceBySuper :: Pred -> TcM [Pred]
reduceBySuper p 
  = do 
      n <- askMaxRecursionDepth
      reduceBySuper' n p 

reduceBySuper' :: Int -> Pred -> TcM [Pred]
reduceBySuper' n p@(InCls c _ _) 
  | n <= 0 = tcmError $ unwords [ "Cannot reduce:"
                                , pretty p  
                                , "since the solver exceeded the max number of iterations."
                                ]
  | otherwise = do 
      ctbl <- getClassEnv 
      case Map.lookup c ctbl of 
        Nothing -> pure [p]
        Just cinfo -> do
           ps' <- concat <$> mapM (reduceBySuper' (n - 1)) (supers cinfo)
           pure (p : ps')

-- entailment 

entails :: [Pred] -> Pred -> TcM Bool 
entails qs p 
  = do 
      n <- askMaxRecursionDepth
      qs' <- mapM reduceBySuper qs
      qs'' <- mapM (reduceByInst n) qs'
      pure $ any (p `elem`) qs''

-- hnf for predicates 

inHnf :: Pred -> Bool 
inHnf (InCls _ t _) = hnf t 
inHnf _ = False

-- head normal form for types 

hnf :: Ty -> Bool 
hnf (TyVar _) = True
hnf _ = False

-- function matches 
-- We assume that we do not support 
-- overlapping instances.

matches :: Pred -> TcM (Maybe (Subst, [Pred]))
matches p@(InCls c t ts) 
  = do 
      insts <- askInstEnv c 
      ms <- catMaybes <$> mapM (genM t) insts 
      case ms of 
        [] -> pure Nothing 
        [x] -> pure (Just x) 
        ins   -> throwError $ unwords [ "Constraint"
                                      , pretty p
                                      , "has more than one matching instance."
                                      ]
matches p = tcmError $ "Invalid constraint:" ++ pretty p

genM :: Ty -> Inst -> TcM (Maybe (Subst, [Pred]))
genM t k@(ps :=> h@(InCls _ t' _)) 
  = do 
      r <- defaultM (match t t')  
      case r of 
        Just s -> pure $ Just (s, apply s ps) 
        Nothing -> pure Nothing

defaultM :: TcM a -> TcM (Maybe a)
defaultM m 
  = do {
      x <- m ;
      pure (Just x)
    } `catchError` (\ _ -> pure Nothing)


