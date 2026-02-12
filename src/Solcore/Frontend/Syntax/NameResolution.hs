module Solcore.Frontend.Syntax.NameResolution where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax.Contract hiding (contracts, decls)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.SyntaxTree qualified as S
import Solcore.Frontend.Syntax.Ty

-- name resolution

nameResolution :: S.CompUnit -> IO (Either String (CompUnit Name))
nameResolution (S.CompUnit imps ds) =
  do
    let genv = globalEnv ds
    r <- runResolveM (resolve ds) genv
    case r of
      Left err -> pure (Left err)
      Right ast -> pure (Right (CompUnit (map resolveImport imps) ast))

resolveImport :: S.Import -> Import
resolveImport (S.Import qn) = Import qn

-- type class for name resolution

class Resolve a where
  type Result a
  resolve :: a -> ResolveM (Result a)

instance (Resolve a) => Resolve [a] where
  type Result [a] = [Result a]

  resolve = mapM resolve

instance (Resolve a) => Resolve (Maybe a) where
  type Result (Maybe a) = Maybe (Result a)

  resolve Nothing = pure Nothing
  resolve (Just x) = Just <$> resolve x

instance Resolve S.TopDecl where
  type Result S.TopDecl = TopDecl Name

  resolve (S.TContr c) =
    TContr <$> withLocalCtx (resolve c)
  resolve (S.TFunDef fd) =
    TFunDef <$> withLocalCtx (resolve fd)
  resolve (S.TClassDef c) =
    TClassDef <$> withLocalCtx (resolve c)
  resolve (S.TInstDef d) =
    TInstDef <$> withLocalCtx (resolve d)
  resolve (S.TDataDef dt) =
    TDataDef <$> withLocalCtx (resolve dt)
  resolve (S.TSym ts) =
    TSym <$> withLocalCtx (resolve ts)
  resolve (S.TPragmaDecl p) = TPragmaDecl <$> resolve p

instance Resolve S.Contract where
  type Result S.Contract = Contract Name

  resolve (S.Contract n vs decls) =
    do
      let ns = map tyconName vs
      mapM_ addTyVar ns
      mapM_ addContractDecl decls
      Contract n (map TVar ns) <$> resolve decls

addContractDecl :: S.ContractDecl -> ResolveM ()
addContractDecl (S.CDataDecl (S.DataTy n _ cons)) =
  do
    addTyCon n
    mapM_ (addDataCon . S.constrName) cons
addContractDecl (S.CFieldDecl (S.Field n _ _)) =
  addField n
addContractDecl (S.CFunDecl (S.FunDef sig _)) =
  addFunctionName (S.sigName sig)
addContractDecl _ = pure ()

instance Resolve S.ContractDecl where
  type Result S.ContractDecl = ContractDecl Name

  resolve (S.CDataDecl dt) =
    CDataDecl <$> resolve dt
  resolve (S.CFieldDecl fd) =
    CFieldDecl <$> resolve fd
  resolve (S.CFunDecl f) =
    CFunDecl <$> resolve f
  resolve (S.CConstrDecl cd) =
    CConstrDecl <$> resolve cd

instance Resolve S.Constructor where
  type Result S.Constructor = Constructor Name

  resolve (S.Constructor ps bdy) =
    withLocalCtx $ do
      ps' <- resolve ps
      let args = map paramName ps'
      mapM_ addParameter args
      bdy' <- resolve bdy
      pure (Constructor ps' bdy')

instance Resolve S.Field where
  type Result S.Field = Field Name

  resolve (S.Field n t me) =
    Field n <$> resolve t <*> resolve me

instance Resolve S.Class where
  type Result S.Class = Class Name

  resolve (S.Class vs ps n ts t sigs) =
    withLocalCtx $ do
      let ns = map tyconName vs
          nt = tyconName t
          nts = map tyconName ts
      unless (elem nt ns) $ do
        undefinedTypeVariables [nt]
      unless (all (flip elem ns) nts) $ do
        undefinedTypeVariables (nts \\ ns)
      mapM_ addTyVar ns
      ps' <- resolve ps
      sigs' <- resolve sigs
      let vs' = map TVar ns
          t' = TVar nt
          ts' = map TVar nts
      pure (Class vs' ps' n ts' t' sigs')

instance Resolve S.Signature where
  type Result S.Signature = Signature Name

  resolve (S.Signature vs ctx n ps mt) =
    withLocalCtx $ do
      let ns = map tyconName vs
      mapM_ addTyVar ns
      ctx' <- resolve ctx
      ps' <- resolve ps
      mt' <- resolve mt
      let vs' = map TVar ns
      pure (Signature vs' ctx' n ps' mt')

instance Resolve S.Instance where
  type Result S.Instance = Instance Name

  resolve (S.Instance d vs ps n ts t funs) =
    withLocalCtx $ do
      let ns = map tyconName vs
      ndt <- lookupClass n
      case ndt of
        Just TClass -> do
          mapM_ addTyVar ns
          ps' <- resolve ps
          ts' <- resolve ts
          t' <- resolve t
          funs' <- resolve funs
          let vs' = map TVar ns
          pure (Instance d vs' ps' n ts' t' funs')
        _ -> undefinedClassError n

instance Resolve S.Param where
  type Result S.Param = Param Name

  resolve (S.Typed n t) = Typed n <$> resolve t
  resolve (S.Untyped n) = pure (Untyped n)

instance Resolve S.Pragma where
  type Result S.Pragma = Pragma

  resolve (S.Pragma t s) =
    Pragma <$> resolve t <*> resolve s

instance Resolve S.PragmaType where
  type Result S.PragmaType = PragmaType

  resolve S.NoCoverageCondition =
    pure NoCoverageCondition
  resolve S.NoPattersonCondition =
    pure NoPattersonCondition
  resolve S.NoBoundVariableCondition =
    pure NoBoundVariableCondition

instance Resolve S.PragmaStatus where
  type Result S.PragmaStatus = PragmaStatus

  resolve S.Enabled = pure Enabled
  resolve S.DisableAll = pure DisableAll
  resolve (S.DisableFor ns) = pure (DisableFor ns)

instance Resolve S.FunDef where
  type Result S.FunDef = FunDef Name

  resolve (S.FunDef (S.Signature vs ctx n ps mt) bds) =
    do
      let ns = map tyconName vs
      withLocalCtx $ do
        mapM_ addTyVar ns
        ctx' <- resolve ctx
        ps' <- resolve ps
        mt' <- resolve mt
        let args = map paramName ps'
        mapM_ addParameter args
        bds' <- resolve bds
        let vs' = map TVar ns
            sig = Signature vs' ctx' n ps' mt'
        pure (FunDef sig bds')

instance Resolve S.Stmt where
  type Result S.Stmt = Stmt Name

  resolve (S.Assign lhs rhs) =
    do
      lhs' <- resolve lhs
      rhs' <- resolve rhs
      pure (lhs' := rhs')
  resolve (S.StmtPlusEq lhs rhs) =
    (:=) <$> resolve lhs <*> resolve (S.ExpPlus lhs rhs)
  resolve (S.StmtMinusEq lhs rhs) =
    (:=) <$> resolve lhs <*> resolve (S.ExpMinus lhs rhs)
  resolve (S.Let n mt me) =
    do
      mt' <- resolve mt
      me' <- resolve me
      addLocalVar n
      pure (Let n mt' me')
  resolve (S.StmtExp e) =
    StmtExp <$> resolve e
  resolve (S.Return e) =
    Return <$> resolve e
  resolve (S.Match es eqns) =
    Match <$> resolve es <*> resolve eqns
  resolve (S.Asm blk) =
    pure (Asm blk)
  resolve (S.If e blk1 blk2) =
    If <$> resolve e <*> resolve blk1 <*> resolve blk2

instance Resolve S.Equation where
  type Result S.Equation = Equation Name

  resolve (ps, blk) =
    withLocalCtx $ do
      (,) <$> resolve ps <*> resolve blk

instance Resolve S.Pat where
  type Result S.Pat = Pat Name

  resolve S.PWildcard = pure PWildcard
  resolve (S.PLit l) = PLit <$> resolve l
  resolve p@(S.Pat n ps) =
    do
      ps' <- resolve ps
      mdt <- lookupName n
      case mdt of
        Just TDataCon -> do
          -- here we desugar tuple patterns into
          -- nested pairs.
          isT <- isTuple p
          pure $
            if isT
              then mkTuplePat ps'
              else PCon n ps'
        _ -> do
          addParameter n
          unless (null ps') $ do
            invalidPatternSyntax (PCon n ps')
          pure (PVar n)

mkTuplePat :: [Pat Name] -> Pat Name
mkTuplePat [] = PCon (Name "()") []
mkTuplePat ps = foldr1 pairPat ps

pairPat :: Pat Name -> Pat Name -> Pat Name
pairPat p1 p2 = PCon (Name "pair") [p1, p2]

isTuple :: S.Pat -> ResolveM Bool
isTuple (S.Pat n ps)
  | n == Name "pair" && length ps /= 1 = pure True
  | n == Name "pair" =
      throwError "Invalid tuple pattern"
isTuple _ = pure False

instance Resolve S.Exp where
  type Result S.Exp = Exp Name

  resolve (S.Lit l) = Lit <$> resolve l
  resolve (S.Lam ps bd mt) =
    withLocalCtx $ do
      ps' <- resolve ps
      mt' <- resolve mt
      let args = map paramName ps'
      mapM_ addParameter args
      bd' <- resolve bd
      pure (Lam ps' bd' mt')
  resolve (S.TyExp e t) =
    TyExp <$> resolve e <*> resolve t
  resolve (S.ExpVar me n) =
    do
      me' <- resolve me
      dt <- lookupName n
      case (me', dt) of
        -- local variables
        (_, Just TLocalVar) -> pure (Var n)
        -- function parameters
        (_, Just TParameter) -> pure (Var n)
        -- field access
        (Nothing, Just TField) ->
          pure (FieldAccess Nothing n)
        -- function reference
        (_, Just TFunction) -> do
          dt1 <- gets (Map.lookup n . fieldEnv)
          case dt1 of
            Just TField -> pure (FieldAccess Nothing n)
            _ -> pure (Var n)
        -- data constructor
        (_, Just TDataCon) -> pure (Con n [])
        -- class name
        (_, Just TClass) -> pure (Var n)
        _ -> undefinedName n
  resolve (S.ExpName me n es) =
    do
      me' <- resolve me
      es' <- resolve es
      dt <- lookupName n
      case (me', dt) of
        -- normal function call
        (Nothing, Just TFunction) ->
          pure (Call Nothing n es')
        -- data constructors
        (Nothing, Just TDataCon) ->
          pure (Con n es')
        (Just (Var d), Just TDataCon) ->
          pure (Con (QualName d (pretty n)) es')
        -- class functions
        (Just (Var c), Just TFunction) -> do
          ct <- lookupName c
          case ct of
            Just TClass ->
              let qn = QualName c (pretty n)
               in pure (Call Nothing qn es')
            _ -> undefinedName c
        (Just (Var c), Nothing) -> do
          ct <- lookupName c
          let qn = QualName c (pretty n)
          cf <- lookupName qn
          case (ct, cf) of
            (Just TClass, Just TFunction) ->
              pure (Call Nothing qn es')
            _ -> undefinedName n
        (Just (Var c), Just TTyVar) -> do
          let qn = QualName c (pretty n)
          cf <- gets (Map.lookup qn . scopeEnv)
          case cf of
            Just TFunction -> pure (Call Nothing qn es')
            _ -> undefinedName n
        -- variables
        (_, Just TLocalVar) ->
          pure (Call Nothing n es')
        (_, Just TParameter) ->
          pure (Call Nothing n es')
        -- error
        _ -> undefinedName n
  resolve (S.ExpPlus e1 e2) =
    do
      e1' <- resolve e1
      e2' <- resolve e2
      let fun = QualName (Name "Add") "add"
      pure $ Call Nothing fun [e1', e2']
  resolve (S.ExpMinus e1 e2) =
    do
      e1' <- resolve e1
      e2' <- resolve e2
      let fun = QualName (Name "Sub") "sub"
      pure $ Call Nothing fun [e1', e2']
  resolve (S.ExpTimes e1 e2) =
    do
      e1' <- resolve e1
      e2' <- resolve e2
      let fun = QualName (Name "Mul") "mul"
      pure $ Call Nothing fun [e1', e2']
  resolve (S.ExpDivide e1 e2) =
    do
      e1' <- resolve e1
      e2' <- resolve e2
      let fun = QualName (Name "Div") "div"
      pure $ Call Nothing fun [e1', e2']
  resolve (S.ExpModulo e1 e2) =
    do
      e1' <- resolve e1
      e2' <- resolve e2
      let fun = QualName (Name "Mod") "mod"
      pure $ Call Nothing fun [e1', e2']
  resolve (S.ExpIndexed array idx) = do
    arr' <- resolve array
    idx' <- resolve idx
    pure $ Indexed arr' idx'
  resolve (S.ExpLT e1 e2) = do
    e1' <- resolve e1
    e2' <- resolve e2
    pure $ Call Nothing (Name "lt") [e1', e2']
  resolve (S.ExpGT e1 e2) = do
    e1' <- resolve e1
    e2' <- resolve e2
    let fun = QualName (Name "Ord") "gt"
    pure $ Call Nothing fun [e1', e2']
  resolve (S.ExpLE e1 e2) = do
    e1' <- resolve e1
    e2' <- resolve e2
    pure $ Call Nothing (Name "le") [e1', e2']
  resolve (S.ExpGE e1 e2) = do
    e1' <- resolve e1
    e2' <- resolve e2
    pure $ Call Nothing (Name "ge") [e1', e2']
  resolve (S.ExpEE e1 e2) = do
    e1' <- resolve e1
    e2' <- resolve e2
    let fun = QualName (Name "Eq") "eq"
    pure $ Call Nothing fun [e1', e2']
  resolve (S.ExpNE e1 e2) = do
    e1' <- resolve e1
    e2' <- resolve e2
    pure $ Call Nothing (Name "ne") [e1', e2']
  resolve (S.ExpLAnd e1 e2) = do
    e1' <- resolve e1
    e2' <- resolve e2
    pure $ Call Nothing (Name "and") [e1', e2']
  resolve (S.ExpLOr e1 e2) = do
    e1' <- resolve e1
    e2' <- resolve e2
    pure $ Call Nothing (Name "or") [e1', e2']
  resolve (S.ExpLNot e) = do
    e' <- resolve e
    pure $ Call Nothing (Name "not") [e']
  resolve (S.ExpCond e1 e2 e3) =
    Cond <$> resolve e1 <*> resolve e2 <*> resolve e3

instance Resolve S.Literal where
  type Result S.Literal = Literal

  resolve (S.IntLit i) = pure (IntLit i)
  resolve (S.StrLit s) = pure (StrLit s)

instance Resolve S.Pred where
  type Result S.Pred = Pred

  resolve (S.InCls n t ts) =
    do
      dt <- lookupClass n
      case dt of
        Just TClass -> do
          t' <- resolve t
          ts' <- resolve ts
          pure (InCls n t' ts')
        _ -> undefinedClassError n

instance Resolve S.DataTy where
  type Result S.DataTy = DataTy

  resolve (S.DataTy n vs cons) =
    withLocalCtx $ do
      mapM_ addTyVar vs'
      cons' <- resolve cons
      pure (DataTy n (map TVar vs') cons')
    where
      vs' = map tyconName vs

instance Resolve S.Constr where
  type Result S.Constr = Constr

  resolve (S.Constr n ts) = Constr n <$> resolve ts

instance Resolve S.TySym where
  type Result S.TySym = TySym

  resolve (S.TySym n ts t) =
    do
      let ts1 = map tyconName ts
      t' <- withLocalCtx $ do
        mapM_ addTyVar ts1
        resolve t
      pure (TySym n (map TVar ts1) t')

tyconName :: S.Ty -> Name
tyconName (S.TyCon n _) = n

instance Resolve S.Ty where
  type Result S.Ty = Ty

  resolve (S.TyCon n ts) =
    do
      ndt <- lookupType n
      case ndt of
        Just TTyCon -> TyCon n <$> resolve ts
        Just TTyVar -> pure (TyVar (TVar n))
        _ -> undefinedTypeConstructor (TyCon n [])

-- definition of an environment

data DeclType
  = TContract
  | TFunction
  | TDataCon
  | TLocalVar
  | TParameter
  | TPattern
  | TField
  | TClass
  | TTyCon
  | TTyVar
  deriving (Show)

data Env
  = Env
  { -- holds types and contracts. global visibility
    typeEnv :: Map Name DeclType,
    -- holds type class names
    classEnv :: Map Name DeclType,
    -- holds field names under a contract scope
    fieldEnv :: Map Name DeclType,
    -- holds names under a specific scope: data constructors, functions
    -- variables and so on.
    scopeEnv :: Map Name DeclType
  }
  deriving (Show)

emptyEnv :: Env
emptyEnv =
  Env
    ( Map.fromList
        [ (Name "word", TTyCon),
          (Name "bool", TTyCon),
          (Name "()", TTyCon),
          (Name "->", TTyCon),
          (Name "pair", TTyCon),
          (Name "sum", TTyCon)
        ]
    )
    (Map.fromList [(Name "invokable", TClass)])
    Map.empty
    ( Map.fromList
        [ (Name "true", TDataCon),
          (Name "false", TDataCon),
          (Name "()", TDataCon),
          (Name "pair", TDataCon),
          (Name "inl", TDataCon),
          (Name "inr", TDataCon),
          (Name "invoke", TFunction),
          (Name "primAddWord", TFunction),
          (Name "primEqWord", TFunction)
        ]
    )

globalEnv :: [S.TopDecl] -> Env
globalEnv = foldr addTopDecl emptyEnv

addTopDecl :: S.TopDecl -> Env -> Env
addTopDecl (S.TContr (S.Contract n _ _)) env =
  env {typeEnv = Map.insert n TContract (typeEnv env)}
addTopDecl (S.TFunDef (S.FunDef sig _)) env =
  env {scopeEnv = Map.insert (S.sigName sig) TFunction (scopeEnv env)}
addTopDecl (S.TClassDef (S.Class _ _ n _ _ sigs)) env =
  let env' =
        foldr
          ( \s ac ->
              let qn = QualName n (pretty (S.sigName s))
               in Map.insert qn TFunction ac
          )
          (scopeEnv env)
          sigs
   in env
        { classEnv = Map.insert n TClass (classEnv env),
          scopeEnv = env'
        }
addTopDecl (S.TDataDef (S.DataTy n _ cons)) env =
  env
    { typeEnv = Map.insert n TTyCon (typeEnv env),
      scopeEnv =
        foldr
          (\d ac -> Map.insert (S.constrName d) TDataCon ac)
          (scopeEnv env)
          cons
    }
addTopDecl (S.TSym (S.TySym n _ _)) env =
  env {typeEnv = Map.insert n TTyCon (typeEnv env)}
addTopDecl _ env = env

-- definition of a monad for name resolution

type ResolveM a = StateT Env (ExceptT String IO) a

runResolveM :: ResolveM a -> Env -> IO (Either String a)
runResolveM m env =
  do
    r <- runExceptT (runStateT m env)
    case r of
      Left err -> pure (Left err)
      Right (x, _) -> pure (Right x)

withLocalCtx :: ResolveM a -> ResolveM a
withLocalCtx m =
  do
    env <- get
    r <- m
    modify
      ( \env1 ->
          env1
            { scopeEnv = scopeEnv env,
              typeEnv = typeEnv env
            }
      )
    pure r

lookupType :: Name -> ResolveM (Maybe DeclType)
lookupType n =
  gets (Map.lookup n . typeEnv)

lookupClass :: Name -> ResolveM (Maybe DeclType)
lookupClass n =
  gets (Map.lookup n . classEnv)

lookupName :: Name -> ResolveM (Maybe DeclType)
lookupName n =
  do
    env <- get
    let ldt = Map.lookup n (scopeEnv env)
        gdt = Map.lookup n (typeEnv env)
        cdt = Map.lookup n (classEnv env)
        fdt = Map.lookup n (fieldEnv env)
    pure (ldt <|> gdt <|> cdt <|> fdt)

wrapError :: (Pretty b) => ResolveM a -> b -> ResolveM a
wrapError m e =
  catchError m handler
  where
    handler msg = throwError (decorate msg)
    decorate msg = msg ++ "\n - in:" ++ pretty e

addContractName :: Name -> ResolveM ()
addContractName n =
  modify (\env -> env {typeEnv = Map.insert n TContract (typeEnv env)})

addFunctionName :: Name -> ResolveM ()
addFunctionName n =
  modify (\env -> env {scopeEnv = Map.insert n TFunction (scopeEnv env)})

addParameter :: Name -> ResolveM ()
addParameter n =
  modify (\env -> env {scopeEnv = Map.insert n TParameter (scopeEnv env)})

addLocalVar :: Name -> ResolveM ()
addLocalVar n =
  modify (\env -> env {scopeEnv = Map.insert n TLocalVar (scopeEnv env)})

addField :: Name -> ResolveM ()
addField n =
  modify (\env -> env {fieldEnv = Map.insert n TField (fieldEnv env)})

addClass :: Name -> ResolveM ()
addClass n =
  modify (\env -> env {classEnv = Map.insert n TClass (classEnv env)})

addTyCon :: Name -> ResolveM ()
addTyCon n =
  modify (\env -> env {typeEnv = Map.insert n TTyCon (typeEnv env)})

addDataCon :: Name -> ResolveM ()
addDataCon n =
  modify (\env -> env {scopeEnv = Map.insert n TDataCon (scopeEnv env)})

addTyVar :: Name -> ResolveM ()
addTyVar n =
  modify (\env -> env {typeEnv = Map.insert n TTyVar (typeEnv env)})

-- error messages

undefinedTypeVariables :: [Name] -> ResolveM a
undefinedTypeVariables ns =
  throwError $ unlines ["Undefined type variables:", unwords (map pretty ns)]

undefinedTypeConstructor :: Ty -> ResolveM a
undefinedTypeConstructor t =
  throwError $ unlines ["Undefined type constructor:", pretty t]

invalidTypeSynonymError :: TySym -> ResolveM a
invalidTypeSynonymError t =
  throwError $ unlines ["Invalid type synonym:", pretty t]

undefinedClassError :: Name -> ResolveM a
undefinedClassError n =
  throwError $ unlines ["Undefined class:", pretty n]

undefinedName :: Name -> ResolveM a
undefinedName n =
  throwError $ unwords ["Undefined name:", pretty n]

invalidPatternSyntax :: Pat Name -> ResolveM a
invalidPatternSyntax p =
  throwError $ unwords ["Invalid pattern syntax:", pretty p]
