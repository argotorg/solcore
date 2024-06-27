module Solcore.Frontend.TypeInference.TcContract where 

import Control.Monad
import Control.Monad.Except

import Data.List
import qualified Data.Map as Map

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcStmt
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives


-- type inference for declarations

tcDecl :: Decl -> TcM ()
tcDecl (FieldDecl fd) = tcField fd
tcDecl (InstDecl id) = tcInstance id 
tcDecl d@(FunDecl _) = tcBindGroup [d]
tcDecl (MutualDecl ds) = tcBindGroup ds 
tcDecl (ConstrDecl cd) = tcConstructor cd 
tcDecl _ = return ()

-- type checking fields

tcField :: Field -> TcM ()
tcField d@(Field n t (Just e)) 
  = do
      -- FIXME: Should we return the constraints?
      (ps', t') <- tcExp e 
      s <- mgu t t' `wrapError` d 
      return () 
tcField (Field _ _ _) = return ()

tcInstance :: Instance -> TcM ()
tcInstance (Instance ctx n ts t funs) 
  = undefined 

tcBindGroup :: [Decl] -> TcM ()
tcBindGroup binds 
  = do 
      funs <- mapM scanFun binds 
      qts <- mapM tcFunDef funs
      qts' <- withCurrentSubst qts 
      schs <- mapM generalize qts'
      let names = map (sigName . funSignature) funs 
          results = zip names schs 
      mapM_ (uncurry extFunEnv) results 

tcFunDef :: FunDef -> TcM ([Pred], Ty)
tcFunDef (FunDef sig bd) 
  = undefined 

scanFun :: Decl -> TcM FunDef 
scanFun (FunDecl (FunDef sig bd)) 
  = flip FunDef bd <$> fillSignature sig 
    where 
      f (Typed n t) = pure $ Typed n t
      f (Untyped n) = Typed n <$> freshTyVar
      fillSignature (Signature ctx n ps t)
        = do 
            ps' <- mapM f ps 
            pure (Signature ctx n ps' t)
scanFun d = throwError $ unlines [ "Invalid declaration in bind-group:"
                                 , pretty d
                                 ]

-- type generalization 

generalize :: ([Pred], Ty) -> TcM Scheme 
generalize (ps,t) 
  = do 
      envVars <- getEnvFreeVars 
      (ps1,t1) <- withCurrentSubst (ps,t)
      ps2 <- reduceContext ps1 
      t2 <- withCurrentSubst t1 
      let vs = fv (ps2,t2)
          sch = Forall (vs \\ envVars) (ps2 :=> t2)
      return sch

-- context reduction 

reduceContext :: [Pred] -> TcM [Pred]
reduceContext preds 
  = do 
      depth <- askMaxRecursionDepth 
      unless (null preds) $ info ["> reduce context ", pretty preds]
      ps1 <- toHnfs depth preds `wrapError` preds
      ps2 <- withCurrentSubst ps1 
      unless (null preds) $ info ["> reduced context ", pretty (nub ps2)]
      pure (nub ps2)

toHnfs :: Int -> [Pred] -> TcM [Pred]
toHnfs depth ps 
  = do 
      s <- getSubst 
      ps' <- simplifyEqualities ps 
      ps2 <- withCurrentSubst ps'
      toHnfs' depth ps2 

simplifyEqualities :: [Pred] -> TcM [Pred]
simplifyEqualities ps = go [] ps where
    go rs [] = return rs
    go rs ((t :~: u) : ps) = do
      phi <- mgu t u
      extSubst phi
      ps' <- withCurrentSubst ps
      rs' <- withCurrentSubst rs
      go rs' ps'
    go rs (p:ps) = go (p:rs) ps

toHnfs' :: Int -> [Pred] -> TcM [Pred]
toHnfs' _ [] = return []
toHnfs' 0 ps = throwError("Max context reduction depth exceeded")
toHnfs' d preds@(p:ps) = do
  let d' = d - 1
  rs1 <- toHnf d' p
  ps' <- withCurrentSubst ps   -- important, toHnf may have extended the subst
  rs2 <- toHnfs' d' ps'
  return (rs1 ++ rs2)

toHnf :: Int -> Pred -> TcM [Pred]
toHnf _ (t :~: u) = do
  subst1 <- mgu t u
  extSubst subst1
  return []
toHnf depth pred@(InCls n _ _)
  | inHnf pred = return [pred]
  | otherwise = do
      ce <- getInstEnv
      is <- askInstEnv n
      case byInstM ce pred of
        Nothing -> throwError ("no instance of " ++ pretty pred
                  ++"\nKnown instances:\n"++ (unlines $ map pretty is))
        Just (preds, subst') -> do
            extSubst subst'
            toHnfs (depth - 1) preds

inHnf :: Pred -> Bool
inHnf (InCls c t args) = hnf t where
  hnf (TyVar _) = True
  hnf (TyCon _ _) = False
inHnf (_ :~: _) = False

byInstM :: InstEnv -> Pred -> Maybe ([Pred], Subst)
byInstM ce p@(InCls i t as) 
  = msum [tryInst it | it <- insts ce i] 
    where
      insts m n = maybe [] id (Map.lookup n m)
      tryInst :: Qual Pred -> Maybe ([Pred], Subst)
      tryInst c@(ps :=> h) =
          case matchPred h p of
            Left _ -> Nothing
            Right u -> let tvs = fv h
                       in  Just (map (apply u) ps, restrict u tvs)

-- type checking contract constructors

tcConstructor :: Constructor -> TcM ()
tcConstructor (Constructor ps bd) 
  = do
      -- building parameters for constructors
      let f (Typed n t) = pure (n, monotype t)
          f (Untyped n) = ((n,) . monotype) <$> freshTyVar
      lctx <- mapM f ps 
      withLocalCtx lctx (mapM_ tcStmt bd) 
  
-- checking class definitions and adding them to environment 

checkClasses :: [Class] -> TcM ()
checkClasses = mapM_ checkClass 

checkClass :: Class -> TcM ()
checkClass (Class ps n vs v sigs) 
  = mapM_ checkSignature sigs 
    where
      checkSignature sig@(Signature f ctx ps mt)
        = do 
            pst <- mapM tyParam ps 
            unless (null ctx && v `elem` fv (funtype pst mt))
                   (throwError $ "invalid class declaration: " ++ unName n)
            addClassMethod (InCls n (TyVar v) (TyVar <$> vs))
                           sig 

addClassMethod :: Pred -> Signature -> TcM ()
addClassMethod p@(InCls _ _ _) (Signature f _ ps t) 
  = do
      tps <- mapM tyParam ps  
      let ty = funtype tps t
          vs = fv ty
      extFunEnv f (Forall vs ([p] :=> ty))
addClassMethod p@(_ :~: _) (Signature n _ _ _) 
  = throwError $ unlines [
                    "Invalid constraint:"
                  , pretty p 
                  , "in class method:"
                  , unName n
                  ]

schemeFromSignature :: Signature -> TcM Scheme
schemeFromSignature (Signature f ctx ps t)
  = do 
      tps <- mapM tyParam ps 
      let ty = funtype tps t
      pure (Forall (fv ty) (ctx :=> ty))

-- checking instances and adding them in the environment

checkInstances :: [Instance] -> TcM ()
checkInstances = mapM_ checkInstance 

checkInstance :: Instance -> TcM ()
checkInstance (Instance ctx n ts t funs)
  = do 
      let ipred = InCls n t ts
      -- checking the coverage condition 
      insts <- askInstEnv n `wrapError` ipred
      checkOverlap ipred insts
      coverage <- askCoverage
      when coverage (checkCoverage n ts t `wrapError` ipred)
      -- checking Patterson condition 
      checkMeasure ctx ipred `wrapError` ipred
      -- checking instance methods
      mapM_ (checkMethod ipred) funs
      let ninst = ctx :=> InCls n t ts 
      -- add to the environment 
      addInstance n ninst 

checkOverlap :: Pred -> [Inst] -> TcM ()
checkOverlap _ [] = pure ()
checkOverlap p@(InCls _ t _) (i:is) 
  = do 
        i' <- renameVars (fv t) i
        case i' of 
          (ps :=> (InCls _ t' _)) -> 
            case mgu t t' of
              Right _ -> throwError (unlines ["instance:"
                                             , pretty p
                                             , "with:"
                                             , pretty i'])
              Left _ -> checkOverlap p is
        return ()

checkCoverage :: Name -> [Ty] -> Ty -> TcM ()
checkCoverage cn ts t 
  = do 
      let strongTvs = fv t 
          weakTvs = fv ts 
          undetermined = weakTvs \\ strongTvs
      unless (null undetermined) $ 
          throwError (unlines [
            "Coverage condition fails for class:"
          , unName cn 
          , "- the type:"
          , pretty t 
          , "does not determine:"
          , intercalate ", " (map pretty undetermined)
          ])

checkMethod :: Pred -> FunDef -> TcM () 
checkMethod ih@(InCls n t ts) (FunDef sig bd) 
  = do
      cn <- askCurrentContract
      -- getting current method signature in class 
      st@(Forall _ (qs :=> _)) <- askFun cn (sigName sig)
      p <- maybeToTcM (unwords [ "Constraint for"
                               , unName n
                               , "not found in type of"
                               , unName $ sigName sig])
                      (findPred n qs)
      -- matching substitution of instance head and class predicate
      s <- liftEither (matchPred p ih) `wrapError` ih
      (qs' :=> ty') <- freshInst st 
      tps <- mapM tyParam (sigParams sig)
      let it = funtype tps (sigReturn sig)
      match it (apply s ty') `wrapError` ih 
      pure ()

tyParam :: Param -> TcM Ty 
tyParam (Typed _ t) = pure t 
tyParam (Untyped _) = freshTyVar

findPred :: Name -> [Pred] -> Maybe Pred 
findPred _ [] = Nothing 
findPred n (p@(InCls n' _ _) : ps) 
  | n == n' = Just p 
  | otherwise = findPred n ps

anfInstance :: Inst -> Inst
anfInstance inst@(q :=> p@(InCls c t [])) = inst
anfInstance inst@(q :=> p@(InCls c t as)) = q ++ q' :=> InCls c t bs 
  where
    q' = zipWith (:~:) bs as
    bs = map TyVar $ take (length as) freshNames
    tvs = fv inst
    freshNames = filter (not . flip elem tvs) (TVar <$> namePool)

-- checking Patterson conditions 

checkMeasure :: [Pred] -> Pred -> TcM ()
checkMeasure ps c 
  = if measure ps < measure c then return () 
    else throwError $ unlines [ "Instance "
                              , pretty c
                              , "does not satisfy the Patterson conditions."]
