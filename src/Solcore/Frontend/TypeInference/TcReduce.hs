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
import Solcore.Frontend.TypeInference.TcMonad hiding (entails)
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify

import Solcore.Pipeline.Options

reduce :: [Pred] -> TcM [Pred]
reduce ps0
  = do 
      n <- askMaxRecursionDepth
      improve ps0
      s <- getSubst
      ps' <- reduceI n (apply s ps0) 
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
        liftIO $ putStrLn $ "reduced by super:" ++ (pretty preds)
        unless (null preds) $ info ["> reduce context ", pretty preds]
        ps1 <- reduceByInst n preds 
        liftIO $ putStrLn $ "reduced by isntances:" ++ pretty ps1
        ps2 <- withCurrentSubst ps1
        unless (null preds) $ info ["< reduced context ", pretty (nub ps2)]
        pure (nub ps2)

-- simplify by entailment 

simplify :: [Pred] -> TcM [Pred]
simplify = loop []
  where 
    loop rs [] = pure rs 
    loop rs (p : ps) = do 
      c <- entails (rs ++ ps) p 
      if c then loop rs ps 
        else loop (p : rs) ps

-- improvement

improve :: [Pred] -> TcM ()
improve [] = pure ()
improve xs@((InCls n t ts0) : ps) 
  = do 
      let (tss1, ps2) = check n t ps
      ss <- mapM (unifyTypes ts0) tss1
      mapM_ extSubst ss 
      improve ps 
improve (_ : ps) = improve ps 

check :: Name -> Ty -> [Pred] -> ([[Ty]], [Pred])
check n t ps 
  = let 
      cs = [c | c@(InCls n' t' _) <- ps, n == n', t == t']
      cs' = ps \\ cs 
    in ([ts' | (InCls _ _ ts') <- cs], cs')

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
        case byInstM ce p of
          Nothing -> pure [p]
          Just (preds, subst', instd) -> do 
            info ["Selected instance:", pretty instd]
            extSubst subst' 
            reduceByInst (n - 1) preds

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
      qs' <- mapM reduceBySuper qs 
      pure $ any (p `elem`) qs'

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


