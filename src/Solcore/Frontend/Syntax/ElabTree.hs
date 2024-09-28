module Solcore.Frontend.Syntax.ElabTree where

import Control.Monad
import Control.Monad.Except 
import Control.Monad.Identity
import Control.Monad.State 

import Data.List
import Data.Maybe (isNothing)

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax.Contract hiding (contracts) 
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import qualified Solcore.Frontend.Syntax.SyntaxTree as S 
import Solcore.Frontend.Syntax.Ty 

-- top level elaboration / name resolution function 

buildAST :: S.CompUnit -> IO (Either String (CompUnit Name))
buildAST t 
  = runElabM t 

-- definition of an environment to hold 
-- module declarations 

data Env 
  = Env {
      contracts :: [Name]
    , functions :: [Name]
    , typeNames :: [Name]
    , fields :: [Name]
    , constructors :: [Name]
    , classes :: [Name]
    } deriving Show

instance Semigroup Env where 
  (Env cs fs ts fd ctrs cls) <> (Env cs' fs' ts' fd' ctrs' cls')
    = Env (cs `union` cs') 
          (fs `union` fs') 
          (ts `union` ts') 
          (fd `union` fd') 
          (ctrs `union` ctrs') 
          (cls `union` cls')

instance Monoid Env where 
  mempty = Env [] [] [Name "word"] [] [] []

-- definition of the monad 

type ElabM a = (ExceptT String (StateT Env IO)) a 

runElabM :: Elab a => a -> IO (Either String (Res a))
runElabM t = fst <$> runStateT (runExceptT (elab t)) (initialEnv t)

isDefinedType :: Name -> ElabM Bool 
isDefinedType n 
  = do 
      ts <- gets typeNames
      cs <- gets contracts
      pure (n `elem` (ts ++ cs))

isDefinedConstr :: Name -> ElabM Bool 
isDefinedConstr n 
  = (n `elem`) <$> gets constructors

isFunDef :: Name -> ElabM Bool 
isFunDef n 
  = (n `elem`) <$> gets functions 

isField :: Name -> ElabM Bool 
isField n 
  = (n `elem`) <$> gets fields

isTyVar :: Ty -> Bool 
isTyVar (TyVar _) = True 
isTyVar _ = False 

class Elab a where 
  type Res a
  initialEnv :: a -> Env
  elab :: a -> ElabM (Res a)

  initialEnv _ = mempty 

instance Elab a => Elab [a] where 
  type Res [a] = [Res a]
  initialEnv [] = mempty 
  initialEnv (d : ds) = initialEnv d <> initialEnv ds
  elab = mapM elab 

instance Elab a => Elab (Maybe a) where 
  type Res (Maybe a) = Maybe (Res a)

  initialEnv (Just x) = initialEnv x 
  initialEnv Nothing = mempty 

  elab Nothing = pure Nothing 
  elab (Just x) = Just <$> (elab x)

instance (Elab a, Elab b) => Elab (a,b) where 
  type Res (a,b) = (Res a, Res b)

  elab (x,y) = (,) <$> elab x <*> elab y

instance Elab S.CompUnit where 
  type Res S.CompUnit = CompUnit Name
  initialEnv (S.CompUnit _ ds) 
    = initialEnv ds
  elab (S.CompUnit imps ds) 
    = CompUnit <$> elab imps <*> elab ds 

instance Elab S.Import where 
  type Res S.Import = Import 
  
  elab (S.Import qn) = pure (Import qn)


instance Elab S.TopDecl where 
  type Res S.TopDecl = TopDecl Name

  initialEnv (S.TContr c) = initialEnv c 
  initialEnv (S.TFunDef fd) = initialEnv fd 
  initialEnv (S.TClassDef c) = initialEnv c
  initialEnv (S.TInstDef d) = initialEnv d
  initialEnv (S.TDataDef d) = initialEnv d 
  initialEnv (S.TSym s) = initialEnv s 
  initialEnv (S.TPragmaDecl _) = mempty 

  elab (S.TContr c) = TContr <$> elab c 
  elab (S.TFunDef fd) = TFunDef <$> elab fd 
  elab (S.TClassDef c) = TClassDef <$> elab c
  elab (S.TInstDef d) = TInstDef <$> elab d
  elab (S.TDataDef d) = TDataDef <$> elab d 
  elab (S.TSym s) = TSym <$> elab s 
  elab (S.TPragmaDecl p) = TPragmaDecl <$> elab p

instance Elab S.Pragma where 
  type Res S.Pragma = Pragma

  elab (S.Pragma t s) 
    = Pragma <$> elab t <*> elab s  

instance Elab S.PragmaType where 
  type Res S.PragmaType = PragmaType

  elab S.NoCoverageCondition
    = pure NoCoverageCondition
  elab S.NoPattersonCondition
    = pure NoPattersonCondition
  elab S.NoBoundVariableCondition 
    = pure NoBoundVariableCondition

instance Elab S.PragmaStatus where 
  type Res S.PragmaStatus = PragmaStatus

  elab S.Enabled = pure Enabled 
  elab S.DisableAll = pure DisableAll
  elab (S.DisableFor ns) = pure (DisableFor ns)

instance Elab S.Contract where 
  type Res S.Contract = Contract Name 

  initialEnv (S.Contract n _ decls) 
    = env {contracts = [n] `union` contracts env}
      where 
        env = initialEnv decls

  elab (S.Contract n ts decls) 
    = do
        ts' <- elab ts 
        decls' <- elab decls 
        vs <- mapM mkTyVar ts' 
        pure (Contract n vs decls')

instance Elab S.DataTy where 
  type Res S.DataTy = DataTy 

  initialEnv (S.DataTy n _ cons) 
    = env {typeNames = [n] `union` typeNames env} <> (initialEnv cons)
      where 
        env = mempty 

  elab (S.DataTy n ts cs) 
    = do 
        ts' <- elab ts 
        cs' <- elab cs 
        vs <- mapM mkTyVar ts'
        pure (DataTy n vs cs')

instance Elab S.Constr where 
  type Res S.Constr = Constr

  initialEnv (S.Constr n _) 
    = env {constructors = [n] `union` constructors env}
      where 
        env = mempty 

  elab (S.Constr n ts) 
    = Constr n <$> elab ts 

instance Elab S.Ty where 
  type Res S.Ty = Ty 

  elab t@(S.TyCon n ts) 
    = do 
        isTy <- isDefinedType n 
        ts' <- elab ts 
        if isTy then pure $ TyCon n ts' 
          else if null ts then 
            pure $ TyVar (TVar n)
          else throwError $ 
              unlines ["Undefined type:"
                      , pretty n
                      , "!!"
                      ]

instance Elab S.Pred where 
  type Res S.Pred = Pred

  elab (S.InCls n t ts) 
    = InCls n <$> elab t <*> elab ts 

instance Elab S.TySym where 
  type Res S.TySym = TySym 

  initialEnv (S.TySym n _ _) 
    = env {typeNames = [n] `union` typeNames env}
      where
        env = mempty 

  elab (S.TySym n vs t) 
    = do 
        vs' <- elab vs 
        t' <- elab t
        vs1 <- mapM mkTyVar vs' 
        pure (TySym n vs1 t')

instance Elab S.Constructor where 
  type Res S.Constructor = Constructor Name 

  elab (S.Constructor ps bds) 
    = Constructor <$> elab ps <*> elab bds 

instance Elab S.Class where 
  type Res S.Class = Class Name 

  initialEnv (S.Class ctx n _ _ sigs)
    = env {classes = [n] `union` classes env} 
      where 
        env = initialEnv sigs
  elab (S.Class ctx n vs v sigs) 
    = do 
        ctx' <- elab ctx 
        vs' <- elab vs
        v' <- elab v 
        allVars <- mapM (liftM not . isDefinedType . S.tyName) (v : vs)
        unless (and allVars) $ 
          throwError $ unlines ["Ill-formed class definition:"
                               , pretty n
                               , "all parameters must be type variables. Found:"
                               , unwords (map pretty (v' : vs'))
                               ]
        vs1 <- mapM mkTyVar vs' 
        v1 <- mkTyVar v' 
        sigs' <- elab sigs 
        pure (Class ctx' n vs1 v1 sigs')

mkTyVar :: Ty -> ElabM Tyvar 
mkTyVar (TyVar v) = pure v 
mkTyVar t = throwError $ "Ill-formed type:" ++ pretty t 

instance Elab S.Signature where 
  type Res S.Signature = Signature Name 

  initialEnv (S.Signature _ _ n _ _) 
    = env {functions = [n] `union` functions env }
      where 
        env = mempty 

  elab (S.Signature vs ctx n ps mt) 
    = do 
        vs' <- elab vs 
        ctx' <- elab ctx 
        ps' <- elab ps 
        mt' <- elab mt 
        unless (all isTyVar vs') $ do 
            throwError $ unlines ["Ill-formed function signature:"
                                 , pretty n 
                                 , "type constructor found where a variable is expected:"
                                 , unwords (map pretty vs')
                                 ]
        vs''<- mapM mkTyVar vs'
        pure (Signature vs'' ctx' n ps' mt')

instance Elab S.Instance where 
  type Res S.Instance = Instance Name 

  elab (S.Instance ctx n ts t funs) 
    = do 
        ctx' <- elab ctx 
        ts' <- elab ts 
        t' <- elab t 
        funs' <- elab funs 
        pure (Instance ctx' n ts' t' funs')

instance Elab S.Field where 
  type Res S.Field = Field Name 

  elab (S.Field n t me) 
    = Field n <$> elab t <*> elab me 

instance Elab S.FunDef where 
  type Res S.FunDef = FunDef Name 

  elab (S.FunDef sig bd) 
    = FunDef <$> elab sig <*> elab bd 

instance Elab S.ContractDecl where 
  type Res S.ContractDecl = ContractDecl Name 

  initialEnv (S.CDataDecl dt) = initialEnv dt 
  initialEnv (S.CFieldDecl fd) = initialEnv fd 
  initialEnv (S.CFunDecl fd) = initialEnv fd 
  initialEnv (S.CConstrDecl c) = initialEnv c 
  initialEnv (S.CSym t) = initialEnv t

  elab (S.CDataDecl dt) 
    = CDataDecl <$> elab dt 
  elab (S.CFieldDecl fd)
    = CFieldDecl <$> elab fd 
  elab (S.CFunDecl fd)
    = CFunDecl <$> elab fd 
  elab (S.CConstrDecl c)
    = CConstrDecl <$> elab c 
  elab (S.CSym t)
    = CSym <$> elab t

instance Elab S.Stmt where 
  type Res S.Stmt = Stmt Name 

  elab (S.Assign lhs rhs)
    = (:=) <$> elab lhs <*> elab rhs 
  elab (S.Let n mt me) 
    = Let n <$> elab mt <*> elab me 
  elab (S.StmtExp e) 
    = StmtExp <$> elab e 
  elab (S.Return e)
    = Return <$> elab e 
  elab (S.Match es eqns)
    = Match <$> elab es <*> elab eqns 
  elab (S.Asm blk)
    = pure (Asm blk)

instance Elab S.Param where 
  type Res S.Param = Param Name 

  elab (S.Typed n t) 
    = Typed n <$> elab t 
  elab (S.Untyped n) 
    = pure (Untyped n)

instance Elab S.Exp where 
  type Res S.Exp = Exp Name 

  elab (S.Lit l) 
    = Lit <$> elab l 
  elab (S.Lam ps bd mt) 
    = Lam <$> elab ps <*> elab bd <*> elab mt 
  elab (S.ExpName me n es) 
    = do 
        me' <- elab me 
        es' <- elab es 
        isCon <- isDefinedConstr n 
        isField <- isField n 
        isFun <- isFunDef n 
        -- condition for valid constructor use 
        if isCon && isNothing me' then 
          pure (Con n es')
        -- condition for valid field use 
        else if isField then 
          pure (FieldAccess me' n)
        -- condition for function call 
        else if isFun then 
          pure (Call me' n es')
        -- condition for variables 
        else if isNothing me' && null es' then 
          pure (Var n)
        else pure (Call me' n es')

instance Elab S.Pat where 
  type Res S.Pat = Pat Name 

  elab S.PWildcard = pure PWildcard
  elab (S.PLit l) = PLit <$> elab l 
  elab (S.Pat n ps) 
    = do 
        ps' <- elab ps 
        isCon <- isDefinedConstr n 
        -- condition for constructors 
        if isCon then 
          pure (PCon n ps')
        -- condition for variables 
        else if null ps then 
          pure (PVar n)
        else throwError $ unlines ["Invalid pattern:"
                                  , pretty (PCon n ps')
                                  ]

instance Elab S.Literal where 
  type Res S.Literal = Literal 

  elab (S.IntLit i) = pure (IntLit i)
  elab (S.StrLit s) = pure (StrLit s)
