module Solcore.Frontend.Syntax.ElabTree where
import Prelude hiding(exp)

import Common.Monad
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import Data.List
import Data.Maybe

import GHC.Stack

import Text.Pretty.Simple

import Solcore.Frontend.Parser.SolcoreParser(mkTupleTy)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax.Contract hiding (contracts)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import qualified Solcore.Frontend.Syntax.SyntaxTree as S
import Solcore.Frontend.Syntax.Ty
import Solcore.Primitives.Primitives

-- top level elaboration / name resolution function

buildAST :: S.CompUnit -> IO (Either String (CompUnit Name), Env)
buildAST t
  = runElabM t

buildAST' :: S.CompUnit -> IO (Either String (CompUnit Name, Env))
buildAST' t
  = runElabM' t

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
    , variables :: [Name]
    , currentContract :: Maybe Name
    , fieldTypes :: [Ty]
    } deriving Show

instance Semigroup Env where
  (Env cs fs ts fd ctrs cls vs cc ftys) <> (Env cs' fs' ts' fd' ctrs' cls' vs' cc' ftys')
    = Env (cs `union` cs')
          (fs `union` fs')
          (ts `union` ts')
          (fd `union` fd')
          (ctrs `union` ctrs')
          (cls `union` cls')
          (vs `union` vs')
          (cc')
          (ftys <> ftys')
instance Monoid Env where
  mempty = Env []
               []
               [Name "word", Name "pair", Name "()", Name "bool"]
               []
               [Name "pair", Name "()", Name "true", Name "false"]
               [Name "invokable"]
               []
               Nothing
               []
-- definition of the monad

type ElabM a = (ExceptT String (StateT Env IO)) a

runElabM' :: (Show a, Elab a) => a -> IO (Either String (Res a, Env))
runElabM' t = do
  (eres, env) <- runElabM t
  case eres of
    Left msg -> pure (Left msg)
    Right res -> pure (Right (res, env))

runElabM :: (Show a, Elab a) => a -> IO (Either String (Res a), Env)
runElabM t = do
  let ienv = initialEnv t
  runStateT (runExceptT (elab t)) ienv

type ContractName = Name

setCurrentContract :: Maybe ContractName -> ElabM ()
setCurrentContract x = modify (\s -> s { currentContract = x })

pushVarsInScope :: [Name] -> ElabM ()
pushVarsInScope ns
  = modify (\ st -> st{variables = ns ++ variables st})

popVarsInScope :: [Name] -> ElabM ()
popVarsInScope ns
  = modify (\ st -> st {variables = filter (`notElem` ns) (variables st)})

isDefinedVar :: Name -> ElabM Bool
isDefinedVar n = (elem n) <$> gets variables

isDefinedType :: Name -> ElabM Bool
isDefinedType n
  = do
      ts <- gets typeNames
      cs <- gets contracts
      let xs = (ts ++ cs)
      pure (n `elem` xs)

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

isClassName :: Maybe S.Exp -> ElabM Bool
isClassName Nothing = pure False
isClassName (Just (S.ExpName _ n _))
  = (n `elem`) <$> gets classes
isClassName (Just (S.ExpVar _ n))
  = (n `elem`) <$> gets classes

contractFields :: S.Contract -> [S.Field]
contractFields c = concatMap extract (S.decls c) where
    extract (S.CFieldDecl f) = [f]
    extract _ = []

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
    = do
        imps' <- elab imps
        ds' <- concat <$> elab ds
        pure (CompUnit imps' ds')

instance Elab S.Import where
  type Res S.Import = Import

  elab (S.Import qn) = pure (Import qn)


instance Elab S.TopDecl where
  type Res S.TopDecl = [TopDecl Name]

  initialEnv (S.TContr c) = initialEnv c
  initialEnv (S.TFunDef fd) = initialEnv fd
  initialEnv (S.TClassDef c) = initialEnv c
  initialEnv (S.TInstDef d) = initialEnv d
  initialEnv (S.TDataDef d) = initialEnv d
  initialEnv (S.TSym s) = initialEnv s
  initialEnv (S.TPragmaDecl _) = mempty

  elab (S.TContr c) = do
      c' <- elab c
      extra <- extraTopDeclsForContract c
      pure $ extra ++ [TContr c']
  elab (S.TFunDef fd) = singleton . TFunDef <$> elab fd
  elab (S.TClassDef c) = singleton . TClassDef <$> elab c
  elab (S.TInstDef d) = singleton . TInstDef <$> elab d
  elab (S.TDataDef d) = singleton . TDataDef <$> elab d
  elab (S.TSym s) = singleton . TSym <$> elab s
  elab (S.TPragmaDecl p) = singleton . TPragmaDecl <$> elab p

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

  elab c@(S.Contract n ts decls)
    = do
        setCurrentContract (Just n)
        ts' <- elab ts
        decls' <- concat <$> elab decls
        vs <- mapM mkTyVar ts'
        setCurrentContract Nothing
        pure (Contract n vs decls')

extraTopDeclsForContract :: S.Contract -> ElabM [TopDecl Name]
extraTopDeclsForContract c@(S.Contract cname ts decls) = do
    let singName = singletonNameForContract cname
    let contractSingDecl = TDataDef $ DataTy singName [] [Constr singName []]
    -- simulate field elaboration to get the elaborated type (and maybe init)
    hypotheticalFields <- mapM elab (contractFields c)

    -- let extraFieldDecls = concat [extraTopDeclsForContractField cname f unit | f <- hypotheticalFields]
    let (fieldTypes, extraFieldDecls) = foldl' (flip contractFieldStep) ([], []) hypotheticalFields
    pure (contractSingDecl:extraFieldDecls)
    where
      hypotheticalElabField :: S.Field -> ElabM (Field Name)
      hypotheticalElabField  (S.Field n t me)
          = Field n <$> elab t <*> elab me

      -- given a list of contract field types so far and topdecls for them, amends them with data for another field
      -- the types of previous fields are needed to construct field offset
      contractFieldStep :: Field Name -> ([Ty], [TopDecl Name]) -> ([Ty], [TopDecl Name])
      contractFieldStep field (tys, decls) = (tys', decls') where
          tys' = tys ++ [fieldTy field]
          decls' = decls ++ extraTopDeclsForContractField cname field offset
          offset = foldr pair unit tys


extraTopDeclsForContractField :: ContractName -> Field Name -> Ty -> [TopDecl Name]
extraTopDeclsForContractField cname field@(Field fname fty _minit) offset = [selDecl, TInstDef sfInstance] where
  -- data b_sel = n_sel
  selName = selectorNameForField fname
  selDecl = TDataDef $ DataTy selName [] [Constr selName []]
  selType = TyCon selName []
  -- instance StructField(ContractStorage(CCtx), fld1_sel):StructField(uint, ()) {}
  ctxTy = TyCon "ContractStorage" [singletonTypeForContract cname]
  sfInstance = Instance
               { instDefault = False
               , instVars = []
               , instContext = []
               , instName = "StructField"
               , paramsTy = [fty, offset]
               , mainTy = TyCon "StructField" [ctxTy, selType]
               , instFunctions = []
               }
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
        isVar <- isDefinedVar n
        isTy <- isDefinedType n
        ts' <- elab ts
        if isVar && null ts then do
          pure $ TyVar (TVar n)
        else if isTy then do
          pure $ TyCon n ts'
            else if null ts then
              pure $ TyVar (TVar n)
            else if isArrow n then
              pure $ TyCon n ts'
            else throwError $
                unlines ["Undefined type:"
                        , pretty n
                        , "!!"
                        ]

isArrow :: Name -> Bool
isArrow (Name s) = s == "->"
isArrow _ = False

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

  initialEnv (S.Class _ ctx n _ _ sigs)
    = env {classes = [n] `union` classes env}
      where
        env = initialEnv sigs
  elab (S.Class bvs ctx n vs v sigs)
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
        let bvs' = map TVar (names bvs)
        pure (Class bvs' ctx' n vs1 v1 sigs')

mkTyVar :: Ty -> ElabM Tyvar
mkTyVar (TyVar v) = pure v
mkTyVar t = throwError $ "Ill-formed type:" ++ pretty t

instance Elab S.Signature where
  type Res S.Signature = Signature Name

  initialEnv (S.Signature _ _ n _ _)
    = env {functions = [n] `union` functions env }
      where
        env = mempty

  elab sig@(S.Signature vs ctx n ps mt)
    = do
        let vs' = map TVar (names vs)
        ctx' <- elab ctx
        ps' <- elab ps
        mt' <- elab mt
        pure (Signature vs' ctx' n ps' mt')

names :: [S.Ty] -> [Name]
names = nub . foldr step []
  where
    step (S.TyCon n ts) ac
      = n : (names ts) ++ ac

instance Elab S.Instance where
  type Res S.Instance = Instance Name

  elab (S.Instance d vs ctx n ts t funs)
    = do
        pushVarsInScope (names vs)
        let vs' = map TVar (names vs)
        ctx' <- elab ctx
        ts' <- elab ts
        t' <- elab t
        funs' <- elab funs
        popVarsInScope (names vs)
        pure (Instance d vs' ctx' n ts' t' funs')


elabContractField :: S.Field -> ElabM [ContractDecl Name]
elabContractField fd@(S.Field fname ft Nothing) = pure [] -- contract fields are desugared elsewhere
elabContractField fd = notImplementedM "elabContractField" fd

selectorNameForField :: Name -> Name  -- FIXME: include contract name
selectorNameForField (Name s) = Name (s <> "_sel")
selectorNameForField n = notImplementedS "selectorNameForField" n

singletonNameForContract :: Name -> Name
singletonNameForContract (Name s) = Name (s <>  "Cxt")

singletonTypeForContract :: Name -> Ty
singletonTypeForContract cname = TyCon (singletonNameForContract cname) []

singletonValForContract :: Name -> Exp Name
singletonValForContract cname = Con (singletonNameForContract cname) []

contractContext :: ElabM (Exp Name)
contractContext = do
  cname <- fromMaybe (error "ElabTree - impossible: assignment outside of contract") <$> gets currentContract
  let singName = singletonNameForContract cname
  let contractSingTy = TyCon singName [] -- FIXME: separate singletons for each contract
  let contractSing = Con singName [] -- FIXME: separate singletons for each contract
  let cxtTy = TyCon "ContractStorage" [contractSingTy]
  let cxt = Con "ContractStorage" [contractSing]
  pure cxt


elabAssignment :: S.Exp -> S.Exp -> ElabM (Stmt Name)
elabAssignment lhs@(S.ExpVar Nothing name) rhs = do
  isF <- isField name
  if isF then elabContractFieldAssignment name rhs
         else (:=) <$> elab lhs <*> elab rhs

elabAssignment lhs@(S.ExpIndexed arr idx) rhs = do
    -- writes [ "> elabAssignment ", show lhs, " <~ ", show rhs]
    idx' <- elab idx
    lhs' <- lhsIndex arr idx'
    rhs' <- elab rhs
    let assignName = QualName (Name "Assign") "assign"
    -- writes [ "< elabAssignment ", pretty lhs', " <~ ", pretty rhs']

    pure $ StmtExp $ Call Nothing assignName [lhs', rhs']

elabAssignment lhs rhs =
    (:=) <$> elab lhs <*> elab rhs

lhsAccess :: Exp Name -> Exp Name
lhsAccess e = Call Nothing (QualName "LVA" "acc") [e]

rhsAccess :: Exp Name -> Exp Name
rhsAccess e = Call Nothing (QualName "RVA" "acc") [e]

indexFun :: Either () () -> Name
indexFun Left{}  = (Name "lidx")
indexFun Right{} = (Name "ridx")

indexAccess :: Either () () -> S.Exp -> Exp Name -> ElabM (Exp Name)
indexAccess dir exp@(S.ExpVar Nothing name) idx = do
  isF <- isField name
  if isF then do
    arrProxy <- memberProxyFor name
    let arrRef = lhsAccess arrProxy
    pure $ Call Nothing (indexFun dir) [arrRef, idx]
  else notImplementedM "indexAccess" exp

indexAccess dir exp@(S.ExpIndexed arr1 idx1) idx2 = do
   idx' <- elab idx1
   arr2 <- lhsIndex arr1 idx'
   pure $ Call Nothing (indexFun dir) [arr2, idx2]

indexAccess _dir exp _idx = notImplementedM "indexAccess" exp

lhsIndex = indexAccess $ Left ()
rhsIndex = indexAccess $ Right ()

memberProxyFor :: Name -> ElabM(Exp Name)
memberProxyFor field = do
  cxt <- contractContext
  let selName = selectorNameForField field
  let selector = Con selName []
  let fieldMap = Con "MemberAccessProxy" [cxt, selector]
  pure fieldMap

elabContractFieldAssignment :: Name -> S.Exp -> ElabM(Stmt Name)
elabContractFieldAssignment field rhs = do
{- Desugaring scheme:
       // this.counter = rhs
       let cxt = ContractStorage(CounterCxt);
       let counter_map : MemberAccessProxy(cxt, counter_sel, ())
       = MemberAccessProxy(cxt, counter_sel);
       let counter_lval : storageRef(word)
                        = LVA.acc(counter_map);
       let counter_rval : word
                        = RVA.acc(counter_map);
       Assign.assign(counter_lval, counter_rval);
-}
  fieldMap <- memberProxyFor field
  let lhs' = lhsAccess fieldMap
  rhs' <- elab rhs
  let assignName = QualName (Name "Assign") "assign"
  pure $ StmtExp $ Call Nothing assignName [lhs', rhs']

instance Elab S.Field where
  type Res S.Field = Field Name

  elab (S.Field n t me)
    = Field n <$> elab t <*> elab me

  initialEnv (S.Field n _t _i)
    = env {fields = [n] `union` fields env }
      where
        env = mempty

instance Elab S.FunDef where
  type Res S.FunDef = FunDef Name

  elab (S.FunDef sig bd)
    = do
         let vs = names (S.sigVars sig)
         pushVarsInScope vs
         sig' <- elab sig
         bd' <- elab bd
         popVarsInScope vs
         pure (FunDef sig' bd')

  initialEnv (S.FunDef sig _) = initialEnv sig

instance Elab S.ContractDecl where
  type Res S.ContractDecl = [ContractDecl Name]

  initialEnv (S.CDataDecl dt) = initialEnv dt
  initialEnv (S.CFieldDecl fd) = initialEnv fd
  initialEnv (S.CFunDecl fd) = initialEnv fd
  initialEnv (S.CConstrDecl c) = initialEnv c

  elab (S.CDataDecl dt)
    = singleton . CDataDecl <$> elab dt
  elab (S.CFieldDecl fd)
    = elabContractField fd -- contract fields are desugared away
  elab (S.CFunDecl fd)
    = singleton . CFunDecl <$> elab fd
  elab (S.CConstrDecl c)
    = singleton . CConstrDecl <$> elab c


instance Elab S.Stmt where
  type Res S.Stmt = Stmt Name

  elab (S.Assign lhs rhs)
    = elabAssignment lhs rhs
  elab (S.StmtPlusEq lhs rhs)
    = elabAssignment lhs (S.ExpPlus lhs rhs)
  elab (S.StmtMinusEq lhs rhs)
    = elabAssignment lhs (S.ExpMinus lhs rhs)
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
  elab (S.If e blk1 blk2)
    = If <$> elab e <*> elab blk1 <*> elab blk2

instance Elab S.Param where
  type Res S.Param = Param Name

  elab (S.Typed n t)
    = Typed n <$> elab t
  elab (S.Untyped n)
    = pure (Untyped n)

mkClassName :: Maybe (Exp Name) -> Name -> Name
mkClassName Nothing n = n
mkClassName (Just (Var x)) n
  = QualName x (pretty n)

instance Elab S.Exp where
  type Res S.Exp = Exp Name

  elab (S.Lit l)
    = Lit <$> elab l
  elab (S.Lam ps bd mt)
    = Lam <$> elab ps <*> elab bd <*> elab mt
  elab (S.TyExp e t)
    = TyExp <$> elab e <*> elab t
  elab (S.ExpVar me n)
    = do
        me' <- elab me
        isF <- isField n
        isCon <- isDefinedConstr n
        if isF then
          case me' of
            Nothing -> do -- this is a contract field
              cxt <- contractContext
              let rvalFun = Name "rval"
              let fieldSel = Con (selectorNameForField n) []
              let fieldMap = Con "MemberAccessProxy" [cxt, fieldSel]
              pure (rhsAccess fieldMap)
            _ -> pure $ FieldAccess me' n -- TODO: structures other than contract context
        else if isCon && isNothing me then pure (Con n [])
             else pure $ Var n
  elab (S.ExpName me n es)
    = do
        me' <- elab me
        es' <- elab es
        isCon <- isDefinedConstr n
        isClass <- isClassName me
        -- condition for valid constructor use
        if isCon && isNothing me' then
          pure (Con n es')
        else if isClass then do 
          pure (Call Nothing (mkClassName me' n) es')
        -- condition for function call
        else pure (Call me' n es')

  elab (S.ExpIndexed arr idx) = do
    idx' <- elab idx
    rhsIndex arr idx'

  elab (S.ExpGE e1 e2) = do
     (e1', e2') <- elab (e1, e2)
     pure $ Call Nothing (Name "ge") [e1', e2']

  elab (S.ExpGT e1 e2) = do
     (e1', e2') <- elab (e1, e2)
     let fun = QualName (Name "Num") "gt"
     pure $ Call Nothing fun [e1', e2']

  elab (S.ExpEE e1 e2) = do
     (e1', e2') <- elab (e1, e2)
     let fun = QualName (Name "Eq") "eq"
     pure $ Call Nothing fun [e1', e2']

  elab (S.ExpNE e1 e2) = do
     (e1', e2') <- elab (e1, e2)
     pure $ Call Nothing (Name "ne") [e1', e2']

  elab (S.ExpLAnd e1 e2) = do
     (e1', e2') <- elab (e1, e2)
     pure $ Call Nothing (Name "and") [e1', e2']

  elab (S.ExpLNot e) = do
     e' <- elab e
     pure $ Call Nothing (Name "not") [e']

  elab (S.ExpPlus e1 e2) = do
     (e1', e2') <- elab (e1, e2)
     let fun = QualName (Name "Num") "add"
     pure $ Call Nothing fun [e1', e2']

  elab (S.ExpMinus e1 e2) = do
     (e1', e2') <- elab (e1, e2)
     let fun = QualName (Name "Num") "sub"
     pure $ Call Nothing fun [e1', e2']
  elab (S.ExpCond e1 e2 e3)
    = Cond <$> elab e1 <*> elab e2 <*> elab e3
  elab exp = notImplementedM "elab @Exp" exp


instance Elab S.Pat where
  type Res S.Pat = Pat Name

  elab S.PWildcard = pure PWildcard
  elab (S.PLit l) = PLit <$> elab l
  elab p@(S.Pat n ps)
    = do
        ps' <- elab ps
        isCon <- isDefinedConstr n
        isT <- isTuple p
        -- condition for tuples
        if isT then
          pure $ mkTuplePat ps'
        else if isCon then
          pure (PCon n ps')
        -- condition for variables
        else if null ps then
          pure (PVar n)
        else throwError $ unlines ["Invalid pattern:"
                                  , pretty (PCon n ps')
                                  ]
mkTuplePat :: [Pat Name] -> Pat Name
mkTuplePat [] = PCon (Name "()") []
mkTuplePat ps = foldr1 pairPat ps

pairPat :: Pat Name -> Pat Name -> Pat Name
pairPat p1 p2 = PCon (Name "pair") [p1, p2]

isTuple :: S.Pat -> ElabM Bool
isTuple (S.Pat n ps)
  | n == Name "pair" && length ps /= 1 = pure True
  | n == Name "pair"
    = throwError "Invalid tuple pattern"
isTuple _ = pure False


instance Elab S.Literal where
  type Res S.Literal = Literal

  elab (S.IntLit i) = pure (IntLit i)
  elab (S.StrLit s) = pure (StrLit s)


notImplemented :: (HasCallStack, Pretty a) => String -> a -> b
notImplemented funName a = error $ concat [funName, " not implemented yet for ", pretty a]

notImplementedS :: (HasCallStack, Show a) => String -> a -> b
notImplementedS funName a = error $ concat [funName, " not implemented yet for ", show(pShow a)]

notImplementedM :: (HasCallStack, Show a, MonadIO m, MonadError String m) => String -> a -> m b
notImplementedM funName a = do
  writes ["Internal error: ", funName, " not implemented yet for:"]
  liftIO (pPrint a)
  throwError "Stop."
