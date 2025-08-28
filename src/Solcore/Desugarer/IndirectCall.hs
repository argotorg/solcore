module Solcore.Desugarer.IndirectCall where


import Control.Monad.State
import qualified Data.Map as Map
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcEnv (primCtx)
import Solcore.Primitives.Primitives

-- top level desugarer

indirectCall :: CompUnit Name -> IO (CompUnit Name, [Name])
indirectCall cunit
  = (, fnames) <$> runIndirectM (desugar cunit)
                                (Env (Map.keys primCtx ++ fnames))
    where
      fnames = collect cunit

-- type class for desugar indirect calls

class Desugar a where
  desugar :: a -> IndirectM a

instance Desugar a => Desugar [a] where
  desugar = mapM desugar

instance Desugar a => Desugar (Maybe a) where
  desugar = mapM desugar

instance Desugar (CompUnit Name) where
  desugar (CompUnit imps ds) = CompUnit imps <$> desugar ds

instance Desugar (TopDecl Name) where
  desugar (TContr c) = TContr <$> desugar c
  desugar (TFunDef f) = TFunDef <$> desugar f
  desugar (TClassDef c) = pure $ TClassDef c
  desugar (TInstDef i) = TInstDef <$> desugar i
  desugar (TDataDef d) = pure $ TDataDef d
  desugar (TSym s) = pure $ TSym s
  desugar (TPragmaDecl d) = pure $ TPragmaDecl d
  desugar (TMutualDef ms) = TMutualDef <$> desugar ms

instance Desugar (Contract Name) where
  desugar (Contract n vs ds)
    = Contract n vs <$> desugar ds

instance Desugar (FunDef Name) where
  desugar (FunDef sig bdy)
    = FunDef sig <$> desugar bdy

instance Desugar (ContractDecl Name) where
  desugar (CFieldDecl fd)
    = CFieldDecl <$> desugar fd
  desugar (CFunDecl fd)
    = CFunDecl <$> desugar fd
  desugar (CMutualDecl ds)
    = CMutualDecl <$> desugar ds
  desugar (CConstrDecl cd)
    = CConstrDecl <$> desugar cd
  desugar d = pure d

instance Desugar (Field Name) where
  desugar (Field n t me)
    = Field n t <$> desugar me

instance Desugar (Constructor Name) where
  desugar (Constructor ps bd)
    = Constructor ps <$> desugar bd

instance Desugar (Stmt Name) where
  desugar (lhs := rhs)
    = (:=) <$> desugar lhs <*> desugar rhs
  desugar (Let n mt me)
    = Let n mt <$> desugar me
  desugar (StmtExp e)
    = StmtExp <$> desugar e
  desugar (Return e)
    = Return <$> desugar e
  desugar (Match es eqn)
    = Match <$> desugar es <*> desugar eqn
  desugar e@(Asm _) = pure e
  desugar (If e blk1 blk2)
    = If <$> desugar e <*> desugar blk1 <*> desugar blk2

instance Desugar (Exp Name) where
  desugar (Con a es)
    = Con a <$> desugar es
  desugar (FieldAccess e f)
    = (flip FieldAccess f) <$> desugar e
  desugar (Lam ps bd t)
    = Lam ps <$> desugar bd <*> pure t
  desugar (TyExp e t)
    = flip TyExp t <$> desugar e
  desugar (Call m n es)
    = do
        m' <- desugar m
        es' <- desugar es
        b <- isDirectCall n
        let qn = QualName invokableName "invoke"
            args' = [Var n, indirectArgs es']
        if b then
          pure $ Call m' n es'
        else
          pure $ Call Nothing qn args'
  desugar x = pure x

instance Desugar (Equation Name) where
  desugar (ps, ss) = (ps,) <$> desugar ss

instance Desugar (Instance Name) where
  desugar (Instance d vs ps n ts t fs)
    = Instance d vs ps n ts t <$> desugar fs

-- building indirect function call arguments

indirectArgs :: [Exp Name] -> Exp Name
indirectArgs [] = Con (Name "()") []
indirectArgs [e] = e
indirectArgs (e : es) = epair e (indirectArgs es)

-- building the initial environment

class Collect a where
  collect :: a -> [Name]

instance Collect a => Collect [a] where
  collect = concatMap collect

instance Collect a => Collect (Maybe a) where
  collect = concatMap collect

instance Collect (CompUnit Name) where
  collect (CompUnit _ ds) = collect ds

instance Collect (TopDecl Name) where
  collect (TContr c) = collect c
  collect (TFunDef fd)
    = [sigName (funSignature fd)]
  collect (TClassDef c) = collect c
  collect (TInstDef _) = []
  collect (TDataDef _) = []
  collect (TSym _) = []
  collect (TPragmaDecl _) = []
  collect (TMutualDef ms) = collect ms

instance Collect (Contract Name) where
  collect (Contract _ _ ds) = collect ds

instance Collect (ContractDecl Name) where
  collect (CFieldDecl _) = []
  collect (CFunDecl fd)
    = [sigName (funSignature fd)]
  collect (CMutualDecl ds) = concatMap collect ds
  collect (CConstrDecl _) = []
  collect _ = []

instance Collect (Class Name) where
  collect c
    = map (qual . sigName) $ signatures c
      where
        qual n = QualName (className c)  (pretty n)

-- definition of a monad for indirect calls

type IndirectM a = StateT Env IO a

data Env = Env {
             funNames :: [Name]
           } deriving Show

runIndirectM :: IndirectM a -> Env -> IO a
runIndirectM m env = evalStateT m env

isDirectCall :: Name -> IndirectM Bool
isDirectCall n = (elem n) <$> gets funNames

addFunctionName :: Name -> IndirectM ()
addFunctionName n
  = modify (\ env -> env {funNames = n : funNames env})

