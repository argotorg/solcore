module Solcore.Frontend.Syntax.NameResolution where

import Common.Pretty
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Solcore.Frontend.Pretty.TreePretty
import Solcore.Frontend.Syntax.Contract hiding (contracts, decls)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.SyntaxTree qualified as S
import Solcore.Frontend.Syntax.Ty

-- name resolution

nameResolution :: S.CompUnit -> IO (Either String (CompUnit Name))
nameResolution (S.CompUnit imps ds) =
  do
    let genv = addImportsToEnv imps (globalEnv ds)
    r <- runResolveM (resolve ds) genv
    case r of
      Left err -> pure (Left err)
      Right ast -> pure (Right (CompUnit (map resolveImport imps) ast))

resolveImport :: S.Import -> Import
resolveImport (S.ImportModule qn) = ImportModule qn
resolveImport (S.ImportAlias qn asName) = ImportAlias qn asName
resolveImport (S.ImportOnly qn items) = ImportOnly qn (resolveItemSelector items)

resolveItemSelector :: S.ItemSelector -> ItemSelector
resolveItemSelector S.SelectAll = SelectAll
resolveItemSelector (S.SelectOnly names) = SelectOnly names

validateDuplicateNamespacesInCompUnit :: S.CompUnit -> Either String ()
validateDuplicateNamespacesInCompUnit (S.CompUnit _ ds) =
  validateDuplicateNamespaces ds

validateDuplicateNamespaces :: [S.TopDecl] -> Either String ()
validateDuplicateNamespaces ds = do
  ensureNoDuplicateNames "type namespace" (topLevelTypeNames ds)
  ensureNoDuplicateNames "class namespace" (topLevelClassNames ds)
  ensureNoDuplicateNames "term namespace" (topLevelTermNames ds)
  mapM_ validateContractDuplicates [c | S.TContr c <- ds]

validateContractDuplicates :: S.Contract -> Either String ()
validateContractDuplicates (S.Contract cname _ decls) = do
  let typeNames = [n | S.CDataDecl (S.DataTy n _ _) <- decls]
      termNames = contractTermNames decls
      context = "contract " ++ pretty cname
  ensureNoDuplicateNamesIn context "type namespace" typeNames
  ensureNoDuplicateNamesIn context "term namespace" termNames

topLevelTypeNames :: [S.TopDecl] -> [Name]
topLevelTypeNames = concatMap collect
  where
    collect (S.TContr (S.Contract n _ _)) = [n]
    collect (S.TDataDef (S.DataTy n _ _)) = [n]
    collect (S.TSym (S.TySym n _ _)) = [n]
    collect _ = []

topLevelClassNames :: [S.TopDecl] -> [Name]
topLevelClassNames = concatMap collect
  where
    collect (S.TClassDef (S.Class _ _ n _ _ _)) = [n]
    collect _ = []

topLevelTermNames :: [S.TopDecl] -> [Name]
topLevelTermNames = concatMap collect
  where
    collect (S.TFunDef (S.FunDef sig _)) = [S.sigName sig]
    collect (S.TDataDef (S.DataTy tyCon _ cons)) =
      map (qualifiedConstructorName tyCon . S.constrName) cons
    collect _ = []

contractTermNames :: [S.ContractDecl] -> [Name]
contractTermNames = concatMap collect
  where
    collect (S.CFunDecl (S.FunDef sig _)) = [S.sigName sig]
    collect (S.CDataDecl (S.DataTy tyCon _ cons)) =
      map (qualifiedConstructorName tyCon . S.constrName) cons
    collect _ = []

qualifiedConstructorName :: Name -> Name -> Name
qualifiedConstructorName tyCon conName =
  QualName tyCon (pretty (constructorLeafName conName))

ensureNoDuplicateNames :: String -> [Name] -> Either String ()
ensureNoDuplicateNames ns = ensureNoDuplicateNamesIn "module" ns

ensureNoDuplicateNamesIn :: String -> String -> [Name] -> Either String ()
ensureNoDuplicateNamesIn ctx ns names =
  case duplicates of
    [] -> pure ()
    xs ->
      Left $
        unlines
          [ "Duplicate declarations in " ++ ns ++ ":",
            "  " ++ ctx,
            unlines (map (\n -> "  " ++ pretty n) xs)
          ]
  where
    counts :: Map Name Int
    counts = Map.fromListWith (+) [(n, 1) | n <- names]
    duplicates =
      [ n
        | (n, c) <- Map.toList counts,
          c > 1
      ]

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

  resolve t@(S.TContr c) =
    TContr <$> withLocalCtx (resolve c) `wrapError` t
  resolve t@(S.TFunDef fd) =
    TFunDef <$> withLocalCtx (resolve fd) `wrapError` t
  resolve t@(S.TClassDef c) =
    TClassDef <$> withLocalCtx (resolve c) `wrapError` t
  resolve t@(S.TInstDef d) =
    TInstDef <$> withLocalCtx (resolve d) `wrapError` t
  resolve t@(S.TDataDef dt) =
    TDataDef <$> withLocalCtx (resolve dt) `wrapError` t
  resolve t@(S.TSym ts) =
    TSym <$> withLocalCtx (resolve ts) `wrapError` t
  resolve (S.TExportDecl (S.Export items)) =
    pure (TExportDecl (Export (resolveItemSelector items)))
  resolve t@(S.TPragmaDecl p) = TPragmaDecl <$> resolve p `wrapError` t

instance Resolve S.Contract where
  type Result S.Contract = Contract Name

  resolve c@(S.Contract n vs decls) =
    do
      let ns = map tyconName vs
      mapM_ addTyVar ns
      mapM_ addContractDecl decls
      Contract n (map TVar ns) <$> resolve decls `wrapError` c

addContractDecl :: S.ContractDecl -> ResolveM ()
addContractDecl (S.CDataDecl (S.DataTy n _ cons)) =
  do
    addTyCon n
    mapM_ (addDataCon n . S.constrName) cons
addContractDecl (S.CFieldDecl (S.Field n _ _)) =
  addField n
addContractDecl (S.CFunDecl (S.FunDef sig _)) =
  addFunctionName (S.sigName sig)
addContractDecl _ = pure ()

instance Resolve S.ContractDecl where
  type Result S.ContractDecl = ContractDecl Name

  resolve d@(S.CDataDecl dt) =
    CDataDecl <$> resolve dt `wrapError` d
  resolve d@(S.CFieldDecl fd) =
    CFieldDecl <$> resolve fd `wrapError` d
  resolve d@(S.CFunDecl f) =
    CFunDecl <$> resolve f `wrapError` d
  resolve d@(S.CConstrDecl cd) =
    CConstrDecl <$> resolve cd `wrapError` d

instance Resolve S.Constructor where
  type Result S.Constructor = Constructor Name

  resolve c@(S.Constructor ps bdy) =
    withLocalCtx $ do
      ps' <- resolve ps `wrapError` c
      let args = map paramName ps'
      mapM_ addParameter args
      bdy' <- resolve bdy `wrapError` c
      pure (Constructor ps' bdy')

instance Resolve S.Field where
  type Result S.Field = Field Name

  resolve f@(S.Field n t me) =
    do
      t' <- resolve t `wrapError` f
      me' <- resolve me `wrapError` f
      pure (Field n t' me')

instance Resolve S.Class where
  type Result S.Class = Class Name

  resolve d@(S.Class vs ps n ts t sigs) =
    withLocalCtx $ do
      let ns = map tyconName vs
          nt = tyconName t
          nts = map tyconName ts
      unless (elem nt ns) $ do
        undefinedTypeVariables [nt]
      unless (all (flip elem ns) nts) $ do
        undefinedTypeVariables (nts \\ ns)
      mapM_ addTyVar ns
      ps' <- resolve ps `wrapError` d
      sigs' <- resolve sigs `wrapError` d
      let vs' = map TVar ns
          t' = TVar nt
          ts' = map TVar nts
      pure (Class vs' ps' n ts' t' sigs')

instance Resolve S.Signature where
  type Result S.Signature = Signature Name

  resolve s@(S.Signature vs ctx n ps mt) =
    withLocalCtx $ do
      let ns = map tyconName vs
      mapM_ addTyVar ns
      ctx' <- resolve ctx `wrapError` s
      ps' <- resolve ps `wrapError` s
      mt' <- resolve mt `wrapError` s
      let vs' = map TVar ns
      pure (Signature vs' ctx' n ps' mt')

instance Resolve S.Instance where
  type Result S.Instance = Instance Name

  resolve i@(S.Instance d vs ps n ts t funs) =
    withLocalCtx $ do
      let ns = map tyconName vs
      ndt <- lookupClass n
      case ndt of
        Just TClass -> do
          mapM_ addTyVar ns
          ps' <- resolve ps `wrapError` i
          ts' <- resolve ts `wrapError` i
          t' <- resolve t `wrapError` i
          funs' <- resolve funs `wrapError` i
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

  resolve f@(S.FunDef (S.Signature vs ctx n ps mt) bds) =
    do
      let ns = map tyconName vs
      withLocalCtx $ do
        mapM_ addTyVar ns
        ctx' <- resolve ctx `wrapError` f
        ps' <- resolve ps `wrapError` f
        mt' <- resolve mt `wrapError` f
        let args = map paramName ps'
        mapM_ addParameter args
        bds' <- resolve bds `wrapError` f
        let vs' = map TVar ns
            sig = Signature vs' ctx' n ps' mt'
        pure (FunDef sig bds')

instance Resolve S.Stmt where
  type Result S.Stmt = Stmt Name

  resolve s@(S.Assign lhs rhs) =
    do
      lhs' <- resolve lhs `wrapError` s
      rhs' <- resolve rhs `wrapError` s
      pure (lhs' := rhs')
  resolve (S.StmtPlusEq lhs rhs) =
    (:=) <$> resolve lhs <*> resolve (S.ExpPlus lhs rhs)
  resolve (S.StmtMinusEq lhs rhs) =
    (:=) <$> resolve lhs <*> resolve (S.ExpMinus lhs rhs)
  resolve s@(S.Let n mt me) =
    do
      mt' <- resolve mt `wrapError` s
      me' <- resolve me `wrapError` s
      addLocalVar n
      pure (Let n mt' me')
  resolve s@(S.StmtExp e) =
    StmtExp <$> resolve e `wrapError` s
  resolve s@(S.Return e) =
    Return <$> resolve e `wrapError` s
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
  resolve p@(S.PatDot n ps) = do
    ps' <- resolve ps `wrapError` p
    pure (PCon (dotConstructorMarker n) ps')
  resolve p@(S.Pat n ps) =
    do
      ps' <- resolve ps `wrapError` p
      mdt <- lookupName n
      case mdt of
        Just TDataCon -> do
          if isPrimitiveConstructor n
            then do
              -- here we desugar tuple patterns into
              -- nested pairs.
              let n' = constructorLeafName n
                  isT = isTuple n'
              pure $
                if isT
                  then mkTuplePat ps'
                  else PCon n' ps'
            else case splitQualifiedName n of
              Just (qualifier, conName) ->
                PCon <$> resolveQualifiedConstructorName qualifier conName <*> pure ps'
              Nothing -> do
                sameName <- isSameNameConstructor n
                if sameName
                  then PCon <$> resolveSameNameConstructorName n <*> pure ps'
                  else
                    if null ps'
                      then do
                        addParameter n
                        pure (PVar n)
                      else unqualifiedConstructorError n
        _ -> do
          case n of
            QualName _ _ -> undefinedName n
            Name _ -> do
              sameName <- isSameNameConstructor n
              if sameName
                then PCon <$> resolveSameNameConstructorName n <*> pure ps'
                else do
                  hasQualified <- hasQualifiedConstructorLeaf n
                  when hasQualified $
                    unqualifiedConstructorError n
                  addParameter n
                  unless (null ps') $ do
                    invalidPatternSyntax p
                  pure (PVar n)

mkTuplePat :: [Pat Name] -> Pat Name
mkTuplePat [] = PCon (Name "()") []
mkTuplePat ps = foldr1 pairPat ps

pairPat :: Pat Name -> Pat Name -> Pat Name
pairPat p1 p2 = PCon (Name "pair") [p1, p2]

constructorLeafName :: Name -> Name
constructorLeafName (QualName _ n) = Name n
constructorLeafName n = n

dotConstructorMarker :: Name -> Name
dotConstructorMarker n = Name ('.' : pretty (constructorLeafName n))

isPrimitiveConstructor :: Name -> Bool
isPrimitiveConstructor n =
  constructorLeafName n `elem` primitiveConstructors
  where
    primitiveConstructors =
      [ Name "true",
        Name "false",
        Name "()",
        Name "pair",
        Name "inl",
        Name "inr"
      ]

splitQualifiedName :: Name -> Maybe (Name, Name)
splitQualifiedName (QualName qualifier conName) =
  Just (qualifier, Name conName)
splitQualifiedName _ = Nothing

hasQualifiedConstructorLeaf :: Name -> ResolveM Bool
hasQualifiedConstructorLeaf (Name n) = do
  senv <- gets scopeEnv
  pure $
    any
      ( \(k, v) ->
          case (k, v) of
            (QualName _ conName, TDataCon) -> conName == n
            _ -> False
      )
      (Map.toList senv)
hasQualifiedConstructorLeaf _ =
  pure False

isSameNameConstructor :: Name -> ResolveM Bool
isSameNameConstructor n = do
  let leaf = constructorLeafName n
  dt <- lookupType leaf
  case dt of
    Just TTyCon -> do
      cdt <- lookupName (qualifiedConstructorName leaf leaf)
      pure (cdt == Just TDataCon)
    _ ->
      pure False

resolveSameNameConstructorName :: Name -> ResolveM Name
resolveSameNameConstructorName n =
  resolveQualifiedConstructorName leaf leaf
  where
    leaf = constructorLeafName n

instance Resolve S.Exp where
  type Result S.Exp = Exp Name

  resolve (S.Lit l) = Lit <$> resolve l
  resolve (S.ExpDotVar n) = pure (Con (dotConstructorMarker n) [])
  resolve e@(S.ExpDotName n es) =
    Con (dotConstructorMarker n) <$> resolve es `wrapError` e
  resolve e@(S.Lam ps bd mt) =
    withLocalCtx $ do
      ps' <- resolve ps `wrapError` e
      mt' <- resolve mt `wrapError` e
      let args = map paramName ps'
      mapM_ addParameter args
      bd' <- resolve bd `wrapError` e
      pure (Lam ps' bd' mt')
  resolve (S.TyExp e t) =
    TyExp <$> resolve e <*> resolve t
  resolve c@(S.ExpVar me n) =
    do
      me' <- resolve me `wrapError` c
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
        (Nothing, Just TDataCon) -> do
          if isPrimitiveConstructor n
            then pure (Con n [])
            else case splitQualifiedName n of
              Just (qualifier, conName) ->
                Con <$> resolveQualifiedConstructorName qualifier conName <*> pure []
              Nothing -> unqualifiedConstructorError n
        (Just (Var d), Just TDataCon) ->
          Con <$> resolveQualifiedConstructorName d n <*> pure []
        (Just (Var d), Just TTyCon) -> do
          let qn = QualName d (pretty n)
          qdt <- lookupName qn
          case qdt of
            Just TFunction -> pure (Var qn)
            Just TDataCon -> Con <$> resolveQualifiedConstructorName d n <*> pure []
            Just TTyCon -> pure (Var qn)
            Just TModule -> pure (Var qn)
            _ -> undefinedName n
        -- class name
        (_, Just TClass) -> pure (Var n)
        -- type constructor used as a constructor qualifier
        (Nothing, Just TTyCon) -> do
          sameName <- isSameNameConstructor n
          if sameName
            then Con <$> resolveSameNameConstructorName n <*> pure []
            else pure (Var n)
        -- imported module qualifier name
        (_, Just TModule) -> pure (Var n)
        -- module-qualified function or constructor reference
        (Just (Var d), Nothing) -> do
          let qn = QualName d (pretty n)
          qdt <- lookupName qn
          case qdt of
            Just TFunction -> pure (Var qn)
            Just TDataCon -> Con <$> resolveQualifiedConstructorName d n <*> pure []
            Just TTyCon -> pure (Var qn)
            Just TModule -> pure (Var qn)
            _ -> do
              let fallback = QualName (constructorLeafName d) (pretty n)
              fdt <- lookupName fallback
              case fdt of
                Just TDataCon -> Con <$> resolveQualifiedConstructorName d n <*> pure []
                _ -> undefinedName n
        _ -> do
          sameName <- isSameNameConstructor n
          if sameName
            then Con <$> resolveSameNameConstructorName n <*> pure []
            else do
              hasQualified <- hasQualifiedConstructorLeaf n
              if hasQualified
                then unqualifiedConstructorError n
                else undefinedName n
  resolve x@(S.ExpName me n es) =
    do
      me' <- resolve me `wrapError` x
      es' <- resolve es `wrapError` x
      dt <- lookupName n
      case (me', dt) of
        -- normal function call
        (Nothing, Just TFunction) ->
          pure (Call Nothing n es')
        (Nothing, Just TTyCon) -> do
          sameName <- isSameNameConstructor n
          if sameName
            then Con <$> resolveSameNameConstructorName n <*> pure es'
            else undefinedName n
        -- data constructors
        (Nothing, Just TDataCon) -> do
          if isPrimitiveConstructor n
            then pure (Con n es')
            else case splitQualifiedName n of
              Just (qualifier, conName) ->
                Con <$> resolveQualifiedConstructorName qualifier conName <*> pure es'
              Nothing -> unqualifiedConstructorError n
        (Just (Var d), Just TDataCon) ->
          Con <$> resolveQualifiedConstructorName d n <*> pure es'
        (Just (Var c), Just TTyCon) -> do
          let qn = QualName c (pretty n)
          qdt <- lookupName qn
          case qdt of
            Just TFunction -> pure (Call Nothing qn es')
            Just TDataCon -> Con <$> resolveQualifiedConstructorName c n <*> pure es'
            _ -> undefinedName n
        -- class functions
        (Just (Var c), Just TFunction) -> do
          ct <- lookupName c
          let qn = QualName c (pretty n)
          case ct of
            Just TClass ->
              pure (Call Nothing qn es')
            Just TModule -> do
              cf <- lookupName qn
              case cf of
                Just TFunction -> pure (Call Nothing qn es')
                Just TDataCon -> Con <$> resolveQualifiedConstructorName c n <*> pure es'
                _ -> undefinedName n
            _ -> undefinedName c
        (Just (Var c), Nothing) -> do
          ct <- lookupName c
          let qn = QualName c (pretty n)
          cf <- lookupName qn
          case (ct, cf) of
            (Just TClass, Just TFunction) ->
              pure (Call Nothing qn es')
            (_, Just TFunction) ->
              pure (Call Nothing qn es')
            (_, Just TDataCon) ->
              Con <$> resolveQualifiedConstructorName c n <*> pure es'
            _ -> do
              let fallback = QualName (constructorLeafName c) (pretty n)
              fdt <- lookupName fallback
              case fdt of
                Just TDataCon ->
                  Con <$> resolveQualifiedConstructorName c n <*> pure es'
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
        _ -> do
          sameName <- isSameNameConstructor n
          if sameName
            then Con <$> resolveSameNameConstructorName n <*> pure es'
            else do
              hasQualified <- hasQualifiedConstructorLeaf n
              if hasQualified
                then unqualifiedConstructorError n
                else undefinedName n
  resolve c@(S.ExpPlus e1 e2) =
    do
      e1' <- resolve e1 `wrapError` c
      e2' <- resolve e2 `wrapError` c
      let fun = QualName (Name "Add") "add"
      pure $ Call Nothing fun [e1', e2']
  resolve c@(S.ExpMinus e1 e2) =
    do
      e1' <- resolve e1 `wrapError` c
      e2' <- resolve e2 `wrapError` c
      let fun = QualName (Name "Sub") "sub"
      pure $ Call Nothing fun [e1', e2']
  resolve c@(S.ExpTimes e1 e2) =
    do
      e1' <- resolve e1 `wrapError` c
      e2' <- resolve e2 `wrapError` c
      let fun = QualName (Name "Mul") "mul"
      pure $ Call Nothing fun [e1', e2']
  resolve c@(S.ExpDivide e1 e2) =
    do
      e1' <- resolve e1 `wrapError` c
      e2' <- resolve e2 `wrapError` c
      let fun = QualName (Name "Div") "div"
      pure $ Call Nothing fun [e1', e2']
  resolve c@(S.ExpModulo e1 e2) =
    do
      e1' <- resolve e1 `wrapError` c
      e2' <- resolve e2 `wrapError` c
      let fun = QualName (Name "Mod") "mod"
      pure $ Call Nothing fun [e1', e2']
  resolve c@(S.ExpIndexed array idx) = do
    arr' <- resolve array `wrapError` c
    idx' <- resolve idx `wrapError` c
    pure $ Indexed arr' idx'
  resolve c@(S.ExpLT e1 e2) = do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    pure $ Call Nothing (Name "lt") [e1', e2']
  resolve c@(S.ExpGT e1 e2) = do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "Ord") "gt"
    pure $ Call Nothing fun [e1', e2']
  resolve c@(S.ExpLE e1 e2) = do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    pure $ Call Nothing (Name "le") [e1', e2']
  resolve c@(S.ExpGE e1 e2) = do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    pure $ Call Nothing (Name "ge") [e1', e2']
  resolve c@(S.ExpEE e1 e2) = do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "Eq") "eq"
    pure $ Call Nothing fun [e1', e2']
  resolve c@(S.ExpNE e1 e2) = do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    pure $ Call Nothing (Name "ne") [e1', e2']
  resolve c@(S.ExpLAnd e1 e2) = do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    pure $ Call Nothing (Name "and") [e1', e2']
  resolve c@(S.ExpLOr e1 e2) = do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    pure $ Call Nothing (Name "or") [e1', e2']
  resolve c@(S.ExpLNot e) = do
    e' <- resolve e `wrapError` c
    pure $ Call Nothing (Name "not") [e']
  resolve (S.ExpCond e1 e2 e3) =
    Cond <$> resolve e1 <*> resolve e2 <*> resolve e3
  resolve (S.ExpAt t) = do
    t' <- resolve t
    pure
      ( TyExp
          (Con (Name "Proxy") [])
          (TyCon (Name "Proxy") [t'])
      )

instance Resolve S.Literal where
  type Result S.Literal = Literal

  resolve (S.IntLit i) = pure (IntLit i)
  resolve (S.StrLit s) = pure (StrLit s)

instance Resolve S.Pred where
  type Result S.Pred = Pred

  resolve p@(S.InCls n t ts) =
    do
      dt <- lookupClass n
      case dt of
        Just TClass -> do
          t' <- resolve t `wrapError` p
          ts' <- resolve ts `wrapError` p
          pure (InCls n t' ts')
        _ -> undefinedClassError n

instance Resolve S.DataTy where
  type Result S.DataTy = DataTy

  resolve d@(S.DataTy n vs cons) =
    withLocalCtx $ do
      mapM_ addTyVar vs'
      cons' <- resolve cons `wrapError` d
      pure (DataTy n (map TVar vs') (map (qualifyConstrName n) cons'))
    where
      vs' = map tyconName vs

qualifyConstrName :: Name -> Constr -> Constr
qualifyConstrName tyCon (Constr conName tys) =
  Constr (qualifiedConstructorName tyCon conName) tys

instance Resolve S.Constr where
  type Result S.Constr = Constr

  resolve (S.Constr n ts) = Constr n <$> resolve ts

instance Resolve S.TySym where
  type Result S.TySym = TySym

  resolve d@(S.TySym n ts t) =
    do
      let ts1 = map tyconName ts
      t' <- withLocalCtx $ do
        mapM_ addTyVar ts1
        resolve t `wrapError` d
      pure (TySym n (map TVar ts1) t')

tyconName :: S.Ty -> Name
tyconName (S.TyCon n _) = n

instance Resolve S.Ty where
  type Result S.Ty = Ty

  resolve tc@(S.TyCon n ts) =
    do
      ndt <- lookupType n
      case ndt of
        Just TTyCon -> TyCon n <$> resolve ts `wrapError` tc
        Just TTyVar -> pure (TyVar (TVar n))
        _ -> undefinedTypeConstructor tc

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
  | TModule
  deriving (Eq, Show)

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

addImportsToEnv :: [S.Import] -> Env -> Env
addImportsToEnv imps env = foldr addImport env imps

addImport :: S.Import -> Env -> Env
addImport (S.ImportModule n) env =
  foldr addModuleName env (modulePrefixes n)
addImport (S.ImportAlias _ n) env =
  addModuleName n env
addImport (S.ImportOnly _ _) env =
  env

modulePrefixes :: Name -> [Name]
modulePrefixes n =
  reverse (go n)
  where
    go q@(QualName p _) = q : go p
    go x = [x]

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
          ( \d ac ->
              Map.insert (qualifiedConstructorName n (S.constrName d)) TDataCon ac
          )
          (scopeEnv env)
          cons
    }
addTopDecl (S.TSym (S.TySym n _ _)) env =
  env {typeEnv = Map.insert n TTyCon (typeEnv env)}
addTopDecl (S.TExportDecl _) env = env
addTopDecl _ env = env

addModuleName :: Name -> Env -> Env
addModuleName n env =
  env {scopeEnv = Map.insertWith (\_ old -> old) n TModule (scopeEnv env)}

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

addDataCon :: Name -> Name -> ResolveM ()
addDataCon tyconName conName =
  modify
    ( \env ->
        env
          { scopeEnv =
              Map.insert (qualifiedConstructorName tyconName conName) TDataCon (scopeEnv env)
          }
    )

addTyVar :: Name -> ResolveM ()
addTyVar n =
  modify (\env -> env {typeEnv = Map.insert n TTyVar (typeEnv env)})

resolveQualifiedConstructorName :: Name -> Name -> ResolveM Name
resolveQualifiedConstructorName qualifier conName =
  do
    let qn = QualName qualifier (pretty conName)
    dt <- lookupName qn
    case dt of
      Just TDataCon -> pure qn
      _ ->
        let fallback = QualName (constructorLeafName qualifier) (pretty conName)
         in do
              fdt <- lookupName fallback
              case fdt of
                Just TDataCon -> pure fallback
                _ -> undefinedName qn

-- error messages

undefinedTypeVariables :: [Name] -> ResolveM a
undefinedTypeVariables ns =
  throwError $ unlines ["Undefined type variables:", unwords (map pretty ns)]

undefinedTypeConstructor :: S.Ty -> ResolveM a
undefinedTypeConstructor t =
  throwError $ unlines ["Undefined type constructor:", pretty t]

invalidTypeSynonymError :: S.TySym -> ResolveM a
invalidTypeSynonymError t =
  throwError $ unlines ["Invalid type synonym:", pretty t]

undefinedClassError :: Name -> ResolveM a
undefinedClassError n =
  throwError $ unlines ["Undefined class:", pretty n]

undefinedName :: Name -> ResolveM a
undefinedName n =
  throwError $ unwords ["Undefined name:", pretty n]

unqualifiedConstructorError :: Name -> ResolveM a
unqualifiedConstructorError n =
  throwError $
    unlines
      [ "Unqualified constructor:",
        pretty n,
        "Use Type.Constructor form."
      ]

invalidPatternSyntax :: S.Pat -> ResolveM a
invalidPatternSyntax p =
  throwError $ unwords ["Invalid pattern syntax:", pretty p]
