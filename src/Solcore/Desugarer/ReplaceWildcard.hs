module Solcore.Desugarer.ReplaceWildcard where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.List

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id

-- replacing wildcards by fresh pattern variables

class ReplaceWildcard a where
  replace :: a -> CompilerM a

instance ReplaceWildcard a => ReplaceWildcard [a] where
  replace = mapM replace

instance ( ReplaceWildcard a
         , ReplaceWildcard b) => ReplaceWildcard (a,b) where
  replace (a,b) = (,) <$> replace a <*> replace b

instance ReplaceWildcard a => ReplaceWildcard (Maybe a) where
  replace Nothing  = pure Nothing
  replace (Just e) = Just <$> replace e

instance ReplaceWildcard (Pat Id) where
  replace v@(PVar _) = return v
  replace (PCon n ps)
    = PCon n <$> replace ps
  replace PWildcard
    = freshPVar
  replace p@(PLit _)
    = return p

instance ReplaceWildcard (Exp Id) where
  replace v@(Var _) = return v
  replace (Con n es)
    = Con n <$> replace es
  replace (FieldAccess e n)
    = (flip FieldAccess n) <$> replace e
  replace e@(Lit _) = return e
  replace (Call me n es)
    = Call <$> (replace me) <*>
               pure n <*>
               replace es
  replace (Lam args bd mt)
    = Lam args <$> replace bd <*> pure mt
  replace (TyExp e ty)
    = flip TyExp ty <$> replace e

instance ReplaceWildcard (Stmt Id) where
  replace (e1 := e2)
    = (e1 :=) <$> replace e2
  replace (Let n t me)
    = Let n t <$> replace me
  replace (StmtExp e)
    = StmtExp <$> replace e
  replace (Return e)
    = Return <$> replace e
  replace (Match es eqns)
    = Match <$> replace es <*> replace eqns
  replace (If e blk1 blk2)
    = If <$> replace e <*> replace blk1 <*> replace blk2
  replace s = pure s

instance ReplaceWildcard (FunDef Id) where
  replace (FunDef sig bd)
    = FunDef sig <$> replace bd

instance ReplaceWildcard (Constructor Id) where
  replace (Constructor ps bd)
    = Constructor ps <$> replace bd

instance ReplaceWildcard (Instance Id) where
  replace (Instance d vs ps n ts m funs)
    = Instance d vs ps n ts m <$> replace funs

instance ReplaceWildcard (TopDecl Id) where
  replace (TFunDef fd)
    = TFunDef <$> replace fd
  replace (TInstDef inst)
    = TInstDef <$> replace inst
  replace d = return d

instance ReplaceWildcard (ContractDecl Id) where
  replace (CFunDecl fd)
    = CFunDecl <$> replace fd
  replace (CConstrDecl c)
    = CConstrDecl <$> replace c
  replace d = pure d

instance ReplaceWildcard (Contract Id) where
  replace (Contract n ts decls)
    = Contract n ts <$> replace decls

-- Compiler monad infra

type CompilerM a
  = ReaderT String (ExceptT String
                   (WriterT [FunDef Id]
                   (StateT Int IO))) a

mkPrefix :: [Name] -> String
mkPrefix = intercalate "_" . map show

inc :: CompilerM Int
inc = do
  i <- get
  put (i + 1)
  return i

freshName :: CompilerM Name
freshName
  = do
        n <- inc
        -- pre <- ask
        return (Name ("var_" ++ show n))

freshId :: CompilerM Id
freshId = Id <$> freshName <*> var
  where
    var = (TyVar . TVar) <$> freshName

freshExpVar :: CompilerM (Exp Id)
freshExpVar
  = Var <$> freshId

freshPVar :: CompilerM (Pat Id)
freshPVar
  = PVar <$> freshId

runCompilerM :: [Name] -> CompilerM a -> IO (Either String a, [FunDef Id])
runCompilerM ns m
  = evalStateT (runWriterT (runExceptT (runReaderT m (mkPrefix ns)))) 0
