module Solcore.Frontend.Syntax.NameResolution where

import Common.Pretty
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Generics (Data, everything, extQ, mkQ)
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (First (..))
import Solcore.Diagnostics (CompilerError (..), Diagnostic (..), DiagnosticCode (..), Label (..), LabelStyle (..), Severity (..), SourceSpan, addDiagnosticNote, diagnosticCompilerError)
import Solcore.Frontend.Pretty.TreePretty
import Solcore.Frontend.Syntax.Contract hiding (contracts, decls)
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.SyntaxTree qualified as S
import Solcore.Frontend.Syntax.Ty

-- name resolution

nameResolution :: S.CompUnit -> IO (Either CompilerError (CompUnit Name))
nameResolution (S.CompUnit imps ds) =
  fmap fst <$> nameResolutionTopDeclSegments imps [ds]

nameResolutionTopDeclSegments ::
  [S.Import] ->
  [[S.TopDecl]] ->
  IO (Either CompilerError (CompUnit Name, [[TopDecl Name]]))
nameResolutionTopDeclSegments imps segments =
  do
    let ds = concat segments
        genv = addImportsToEnv imps (globalEnv ds)
    r <- runResolveM (mapM resolve segments) genv
    case r of
      Left err -> pure (Left err)
      Right resolvedSegments ->
        let resolvedImports = map resolveImport imps
         in pure (Right (CompUnit resolvedImports (concat resolvedSegments), resolvedSegments))

resolveImport :: S.Import -> Import
resolveImport (S.ImportModule qn) = ImportModule (resolveModulePath qn)
resolveImport (S.ImportAlias qn asName) = ImportAlias (resolveModulePath qn) asName
resolveImport (S.ImportOnly qn items) = ImportOnly (resolveModulePath qn) (resolveItemSelector items)

resolveModulePath :: S.ModulePath -> ModulePath
resolveModulePath (S.RelativePath path) = RelativePath path
resolveModulePath (S.LibraryPath path) = LibraryPath path
resolveModulePath (S.ExternalPath libName path) = ExternalPath libName path

resolveItemSelector :: S.ItemSelector -> ItemSelector
resolveItemSelector (S.SelectItems items hidden) =
  SelectItems (map resolveSelectorEntry items) hidden

resolveConstructorSelector :: S.ConstructorSelector -> ConstructorSelector
resolveConstructorSelector (S.SelectConstructors names) =
  SelectConstructors names
resolveConstructorSelector S.SelectAllConstructors =
  SelectAllConstructors

resolveExportSelector :: S.ExportSelector -> ExportSelector
resolveExportSelector (S.SelectExportItems items) =
  SelectExportItems (map resolveExportSelectorEntry items)

resolveExportSelectorEntry :: S.ExportSelectorEntry -> ExportSelectorEntry
resolveExportSelectorEntry S.SelectExportAllItems =
  SelectExportAllItems
resolveExportSelectorEntry (S.SelectExportItem itemName) =
  SelectExportItem itemName
resolveExportSelectorEntry (S.SelectExportConstructors typeName ctorSelector) =
  SelectExportConstructors typeName (resolveConstructorSelector ctorSelector)

resolveSelectorEntry :: S.ItemSelectorEntry -> ItemSelectorEntry
resolveSelectorEntry S.SelectAllItems = SelectAllItems
resolveSelectorEntry (S.SelectItem itemName) = SelectItem itemName
resolveSelectorEntry (S.SelectItemAs itemName aliasName) = SelectItemAs itemName aliasName

resolveExportSpec :: S.ExportSpec -> ExportSpec
resolveExportSpec S.ExportAll = ExportAll
resolveExportSpec (S.ExportName itemName) = ExportName itemName
resolveExportSpec (S.ExportNameWithConstructors typeName ctorSelector) =
  ExportNameWithConstructors typeName (resolveConstructorSelector ctorSelector)
resolveExportSpec (S.ExportModuleAll path) = ExportModuleAll (resolveModulePath path)

validateDuplicateNamespacesInCompUnit :: S.CompUnit -> Either CompilerError ()
validateDuplicateNamespacesInCompUnit (S.CompUnit _ ds) =
  validateDuplicateNamespaces ds

validateDuplicateNamespacesInTopDeclSegments :: [[S.TopDecl]] -> Either CompilerError ()
validateDuplicateNamespacesInTopDeclSegments segments = do
  ensureNoDuplicateNames "type namespace" (concatMap topLevelTypeNames segments)
  ensureNoDuplicateNames "term namespace" (concatMap topLevelTermNames segments)
  mapM_ validateContractDuplicates [c | segment <- segments, S.TContr c <- segment]

validateDuplicateNamespaces :: [S.TopDecl] -> Either CompilerError ()
validateDuplicateNamespaces ds = do
  ensureNoDuplicateNames "type namespace" (topLevelTypeNames ds)
  ensureNoDuplicateNames "term namespace" (topLevelTermNames ds)
  mapM_ validateContractDuplicates [c | S.TContr c <- ds]

validateContractDuplicates :: S.Contract -> Either CompilerError ()
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
    collect (S.TClassDef (S.Class _ _ n _ _ _)) = [n]
    collect _ = []

topLevelTermNames :: [S.TopDecl] -> [Name]
topLevelTermNames = concatMap collect
  where
    collect (S.TFunDef (S.FunDef _ sig _)) = [S.sigName sig]
    collect (S.TDataDef (S.DataTy tyCon _ cons)) =
      map (qualifiedConstructorName tyCon . S.constrName) cons
    collect _ = []

contractTermNames :: [S.ContractDecl] -> [Name]
contractTermNames = concatMap collect
  where
    collect (S.CFunDecl (S.FunDef _ sig _)) = [S.sigName sig]
    collect (S.CDataDecl (S.DataTy tyCon _ cons)) =
      map (qualifiedConstructorName tyCon . S.constrName) cons
    collect _ = []

qualifiedConstructorName :: Name -> Name -> Name
qualifiedConstructorName tyCon conName =
  qualifyName tyCon (constructorLeafName conName)

ensureNoDuplicateNames :: String -> [Name] -> Either CompilerError ()
ensureNoDuplicateNames ns = ensureNoDuplicateNamesIn "module" ns

ensureNoDuplicateNamesIn :: String -> String -> [Name] -> Either CompilerError ()
ensureNoDuplicateNamesIn ctx ns names =
  case duplicates of
    [] -> pure ()
    xs ->
      Left $
        diagnosticCompilerError $
          diagnosticValue
            "SC0108"
            ("duplicate declarations in " ++ ns)
            []
            (("context: " ++ ctx) : map (\n -> "  " ++ pretty n) xs)
            ["rename or remove the duplicate declaration"]
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

locatedLike :: (HasSourceSpan source) => source -> (SourceSpan -> target -> target) -> target -> target
locatedLike source locate target =
  maybe target (`locate` target) (sourceSpanOf source)

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
  resolve (S.TExportDecl exportDecl) =
    pure (TExportDecl (resolveExport exportDecl))
  resolve t@(S.TPragmaDecl p) = TPragmaDecl <$> resolve p `wrapError` t

resolveExport :: S.Export -> Export
resolveExport (S.ExportList items) =
  ExportList (map resolveExportSpec items)
resolveExport (S.ExportModule path) =
  ExportModule (resolveModulePath path)
resolveExport (S.ExportModuleAs path asName) =
  ExportModuleAs (resolveModulePath path) asName
resolveExport (S.ExportItemsFrom path items) =
  ExportItemsFrom (resolveModulePath path) (resolveExportSelector items)

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
addContractDecl (S.CFunDecl (S.FunDef _ sig _)) =
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

  resolve c@(S.Constructor ps bdy payable) =
    withLocalCtx $ do
      ps' <- resolve ps `wrapError` c
      let args = map paramName ps'
      mapM_ addParameter args
      bdy' <- resolve bdy `wrapError` c
      pure (Constructor ps' bdy' payable)

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

  resolve s@(S.Signature vs ctx n ps rc mt pay) =
    withLocalCtx $ do
      let ns = map tyconName vs
      mapM_ addTyVar ns
      ctx' <- resolve ctx `wrapError` s
      ps' <- resolve ps `wrapError` s
      mt' <- resolve mt `wrapError` s
      let vs' = map TVar ns
      pure (Signature vs' ctx' n ps' rc mt' pay)

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

  resolve (S.Typed c n t) = Typed c n <$> resolve t
  resolve (S.Untyped c n) = pure (Untyped c n)

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
  resolve S.NoGenericInstanceFor =
    pure NoGenericInstanceFor

instance Resolve S.PragmaStatus where
  type Result S.PragmaStatus = PragmaStatus

  resolve S.Enabled = pure Enabled
  resolve S.DisableAll = pure DisableAll
  resolve (S.DisableFor ns) = pure (DisableFor ns)

instance Resolve S.FunDef where
  type Result S.FunDef = FunDef Name

  resolve f@(S.FunDef isPub (S.Signature vs ctx n ps rc mt pay) bds) =
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
            sig = Signature vs' ctx' n ps' rc mt' pay
        pure (FunDef isPub sig bds')

instance Resolve S.Stmt where
  type Result S.Stmt = Stmt Name

  resolve s@(S.Assign lhs rhs) =
    locatedLike s locatedStmt <$> do
      lhs' <- resolve lhs `wrapError` s
      rhs' <- resolve rhs `wrapError` s
      pure (lhs' := rhs')
  resolve s@(S.StmtPlusEq lhs rhs) =
    locatedLike s locatedStmt <$> ((:=) <$> resolve lhs <*> resolve (S.ExpPlus lhs rhs))
  resolve s@(S.StmtMinusEq lhs rhs) =
    locatedLike s locatedStmt <$> ((:=) <$> resolve lhs <*> resolve (S.ExpMinus lhs rhs))
  resolve s@(S.StmtBXorEq lhs rhs) =
    locatedLike s locatedStmt <$> ((:=) <$> resolve lhs <*> resolve (S.ExpBXor lhs rhs))
  resolve s@(S.StmtBAndEq lhs rhs) =
    locatedLike s locatedStmt <$> ((:=) <$> resolve lhs <*> resolve (S.ExpBAnd lhs rhs))
  resolve s@(S.StmtBOrEq lhs rhs) =
    locatedLike s locatedStmt <$> ((:=) <$> resolve lhs <*> resolve (S.ExpBOr lhs rhs))
  resolve s@(S.StmtModEq lhs rhs) =
    locatedLike s locatedStmt <$> ((:=) <$> resolve lhs <*> resolve (S.ExpModulo lhs rhs))
  resolve s@(S.Let c n mt me) =
    locatedLike s locatedStmt <$> do
      mt' <- resolve mt `wrapError` s
      me' <- resolve me `wrapError` s
      addLocalVar n
      pure (Let c n mt' me')
  resolve s@(S.Block blk) =
    locatedLike s locatedStmt <$> withLocalCtx (Block <$> resolve blk)
  resolve s@(S.StmtExp e) =
    locatedLike s locatedStmt <$> (StmtExp <$> resolve e `wrapError` s)
  resolve s@(S.Return e) =
    locatedLike s locatedStmt <$> (Return <$> resolve e `wrapError` s)
  resolve s@(S.Match es eqns) =
    locatedLike s locatedStmt <$> (Match <$> resolve es <*> resolve eqns)
  resolve s@(S.Asm blk) =
    pure (locatedLike s locatedStmt (Asm blk))
  resolve s@(S.If e blk1 blk2) =
    locatedLike s locatedStmt <$> (If <$> resolve e <*> resolve blk1 <*> resolve blk2)
  resolve s@(S.For initStmt cond postStmt body) =
    locatedLike s locatedStmt <$> (For <$> resolve initStmt <*> resolve cond <*> resolve postStmt <*> resolve body)
  resolve s@S.Break = pure (locatedLike s locatedStmt Break)
  resolve s@S.Continue = pure (locatedLike s locatedStmt Continue)
  resolve s@S.EmptyStmt = pure (locatedLike s locatedStmt EmptyStmt)

instance Resolve S.Equation where
  type Result S.Equation = Equation Name

  resolve (ps, blk) =
    withLocalCtx $ do
      (,) <$> resolve ps <*> resolve blk

instance Resolve S.Pat where
  type Result S.Pat = Pat Name

  resolve p@S.PWildcard = pure (locatedLike p locatedPat PWildcard)
  resolve p@(S.PLit l) = locatedLike p locatedPat <$> (PLit <$> resolve l)
  resolve p@(S.PExp e) = locatedLike p locatedPat <$> (PExp <$> resolve e)
  resolve p@(S.PatDot n ps) = do
    ps' <- resolve ps `wrapError` p
    pure (locatedLike p locatedPat (PCon (dotConstructorMarker n) ps'))
  resolve p@(S.Pat n ps) =
    locatedLike p locatedPat <$> do
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
            QualName qualifier conName ->
              PCon <$> resolveQualifiedConstructorName qualifier (Name conName) <*> pure ps'
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
constructorLeafName q@(QualName _ n) = copyNameSourceSpan q (Name n)
constructorLeafName n = n

dotConstructorMarker :: Name -> Name
dotConstructorMarker n = copyNameSourceSpan n (Name ('.' : pretty (constructorLeafName n)))

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
splitQualifiedName q@(QualName qualifier conName) =
  Just (qualifier, copyNameSourceSpan q (Name conName))
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

-- A receiver like @Error@ in @Error.Empty@ is first parsed as an expression
-- on its own and resolved before the outer member-access context is known.
-- When the type has a same-name constructor (@data Error = Error(...) | ...@),
-- the inner resolver eagerly produces @Con (QualName Error Error) []@ for the
-- bare name. Treat that shape as if it were @Var Error@ when it appears in
-- qualifier position so the outer qualifier-handling cases still match.
unwrapQualifierReceiver :: Maybe (Exp Name) -> Maybe (Exp Name)
unwrapQualifierReceiver (Just (Con (QualName d conName) []))
  | pretty d == conName = Just (Var d)
unwrapQualifierReceiver me = me

-- UFCS receiver test.
--
-- A receiver is eligible for UFCS method-call rewriting only when it is an
-- unqualified contract-field access (FieldAccess Nothing _), i.e. a bare
-- field name like members used as members.push(addr).  Restricting the
-- rule to that single shape is what keeps UFCS from colliding with the other
-- meanings of dot syntax:
--
--   - Class.method(args) / Module.func(args): the receiver resolves to a
--     class/module name (Var c), matched by the qualified-name cases in
--     resolveExp (S.ExpName ...) before the UFCS case is reached.
--   - Type.Constructor: parsed as S.ExpDotName, a different surface node.
--   - a plain field read members: no receiver at all.
--   - a call on a local variable x.m(args): x resolves to Var, not a
--     field access, so UFCS does not apply.
--
-- So UFCS is a strict fallback: it only kicks in for field.method(args) once
-- every qualified interpretation has been ruled out.
isUfcsReceiver :: Exp Name -> Bool
isUfcsReceiver (FieldAccess Nothing _) = True
isUfcsReceiver _ = False

instance Resolve S.Exp where
  type Result S.Exp = Exp Name

  resolve e = locatedLike e locatedExp <$> resolveExp e

resolveExp :: S.Exp -> ResolveM (Exp Name)
resolveExp (S.Lit l) = Lit <$> resolve l
resolveExp e@(S.ExpDotName n es) =
  Con (dotConstructorMarker n) <$> resolve es `wrapError` e
resolveExp e@(S.Lam ps bd mt) =
  withLocalCtx $ do
    ps' <- resolve ps `wrapError` e
    mt' <- resolve mt `wrapError` e
    let args = map paramName ps'
    mapM_ addParameter args
    bd' <- resolve bd `wrapError` e
    pure (Lam ps' bd' mt')
resolveExp (S.TyExp e t) =
  TyExp <$> resolve e <*> resolve t
resolveExp c@(S.ExpVar me n) =
  do
    me' <- unwrapQualifierReceiver <$> (resolve me `wrapError` c)
    dt <- lookupName n
    case (me', dt) of
      -- local variables and function parameters (unqualified only)
      (Nothing, Just TLocalVar) -> pure (Var n)
      (Nothing, Just TParameter) -> pure (Var n)
      -- qualified access: qualifier takes precedence over local variable/parameter in scope
      (Just (Var d), Just dt') | dt' `elem` [TLocalVar, TParameter] -> do
        ct <- lookupName d
        let qn = qualifyName d n
        case ct of
          Just TClass -> pure (Var qn)
          Just TModule -> do
            qdt <- lookupName qn
            case qdt of
              Just TFunction -> pure (Var qn)
              Just TDataCon -> Con <$> resolveQualifiedConstructorName d n <*> pure []
              _ -> undefinedName qn
          _ -> pure (Var n)
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
        let qn = qualifyName d n
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
        let qn = qualifyName d n
        qdt <- lookupName qn
        case qdt of
          Just TFunction -> pure (Var qn)
          Just TDataCon -> Con <$> resolveQualifiedConstructorName d n <*> pure []
          Just TTyCon -> pure (Var qn)
          Just TModule -> pure (Var qn)
          _ -> do
            let fallback = qualifyName (constructorLeafName d) n
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
resolveExp x@(S.ExpName me n es) =
  do
    me' <- unwrapQualifierReceiver <$> (resolve me `wrapError` x)
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
        let qn = qualifyName c n
        qdt <- lookupName qn
        case qdt of
          Just TFunction -> pure (Call Nothing qn es')
          Just TDataCon -> Con <$> resolveQualifiedConstructorName c n <*> pure es'
          _ -> undefinedName n
      -- class functions
      (Just (Var c), Just TFunction) -> do
        ct <- lookupName c
        let qn = qualifyName c n
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
        let qn = qualifyName c n
        cf <- lookupName qn
        case (ct, cf) of
          (Just TClass, Just TFunction) ->
            pure (Call Nothing qn es')
          (_, Just TFunction) ->
            pure (Call Nothing qn es')
          (_, Just TDataCon) ->
            Con <$> resolveQualifiedConstructorName c n <*> pure es'
          _ -> do
            let fallback = qualifyName (constructorLeafName c) n
            fdt <- lookupName fallback
            case fdt of
              Just TDataCon ->
                Con <$> resolveQualifiedConstructorName c n <*> pure es'
              _ ->
                -- UFCS-style method call on a value receiver (local variable or
                -- function parameter): x.method(args) -> Class.method(x, args)
                -- when a unique class exposes `method`. Contract-field receivers
                -- take the isUfcsReceiver path; this extends the same sugar to
                -- ordinary values (e.g. a calldata-array parameter `ops.length()`).
                -- Ambiguity across classes makes findClassWithMethod return
                -- Nothing and falls through to the undefined-name error.
                case ct of
                  Just dt' | dt' `elem` [TLocalVar, TParameter] -> do
                    mClass <- findClassWithMethod n
                    case mClass of
                      Just cls -> pure (Call Nothing (qualifyName cls n) (Var c : es'))
                      Nothing -> undefinedName n
                  _ -> undefinedName n
      (Just (Var c), Just TTyVar) -> do
        let qn = qualifyName c n
        cf <- gets (Map.lookup qn . scopeEnv)
        case cf of
          Just TFunction -> pure (Call Nothing qn es')
          _ -> undefinedName n
      -- variables (unqualified only)
      (Nothing, Just TLocalVar) ->
        pure (Call Nothing n es')
      (Nothing, Just TParameter) ->
        pure (Call Nothing n es')
      -- qualified access: qualifier takes precedence over local variable/parameter in scope
      (Just (Var d), Just TLocalVar) -> do
        ct <- lookupName d
        let qn = qualifyName d n
        case ct of
          Just TClass -> pure (Call Nothing qn es')
          Just TModule -> do
            qdt <- lookupName qn
            case qdt of
              Just TFunction -> pure (Call Nothing qn es')
              Just TDataCon -> Con <$> resolveQualifiedConstructorName d n <*> pure es'
              _ -> undefinedName n
          _ -> pure (Call Nothing n es')
      (Just (Var d), Just TParameter) -> do
        ct <- lookupName d
        let qn = qualifyName d n
        case ct of
          Just TClass -> pure (Call Nothing qn es')
          Just TModule -> do
            qdt <- lookupName qn
            case qdt of
              Just TFunction -> pure (Call Nothing qn es')
              Just TDataCon -> Con <$> resolveQualifiedConstructorName d n <*> pure es'
              _ -> undefinedName n
          _ -> pure (Call Nothing n es')
      -- UFCS-style method call on a contract-field receiver:
      -- field.method(args) -> Class.method(field, args) when a unique class
      -- exposes a method named n.  This is the last case before the error
      -- fallback, so it only fires once every qualified interpretation has been
      -- ruled out (see isUfcsReceiver).  Ambiguity across several classes
      -- makes findClassWithMethod return Nothing and falls through to the
      -- undefined-name error rather than guessing.
      (Just receiver, _) | isUfcsReceiver receiver -> do
        mClass <- findClassWithMethod n
        case mClass of
          Just c -> pure (Call Nothing (qualifyName c n) (receiver : es'))
          Nothing -> undefinedName n
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
resolveExp c@(S.ExpPlus e1 e2) =
  do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "Add") "add"
    pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpMinus e1 e2) =
  do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "Sub") "sub"
    pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpTimes e1 e2) =
  do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "Mul") "mul"
    pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpDivide e1 e2) =
  do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "Div") "div"
    pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpModulo e1 e2) =
  do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "Mod") "mod"
    pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpBXor e1 e2) =
  do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "BitXor") "bxor"
    pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpBAnd e1 e2) =
  do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "BitAnd") "band"
    pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpBOr e1 e2) =
  do
    e1' <- resolve e1 `wrapError` c
    e2' <- resolve e2 `wrapError` c
    let fun = QualName (Name "BitOr") "bor"
    pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpIndexed array idx) = do
  arr' <- resolve array `wrapError` c
  idx' <- resolve idx `wrapError` c
  pure $ Indexed arr' idx'
resolveExp c@(S.ExpLT e1 e2) = do
  e1' <- resolve e1 `wrapError` c
  e2' <- resolve e2 `wrapError` c
  pure $ Call Nothing (Name "lt") [e1', e2']
resolveExp c@(S.ExpGT e1 e2) = do
  e1' <- resolve e1 `wrapError` c
  e2' <- resolve e2 `wrapError` c
  let fun = QualName (Name "Ord") "gt"
  pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpLE e1 e2) = do
  e1' <- resolve e1 `wrapError` c
  e2' <- resolve e2 `wrapError` c
  pure $ Call Nothing (Name "le") [e1', e2']
resolveExp c@(S.ExpGE e1 e2) = do
  e1' <- resolve e1 `wrapError` c
  e2' <- resolve e2 `wrapError` c
  pure $ Call Nothing (Name "ge") [e1', e2']
resolveExp c@(S.ExpEE e1 e2) = do
  e1' <- resolve e1 `wrapError` c
  e2' <- resolve e2 `wrapError` c
  let fun = QualName (Name "Eq") "eq"
  pure $ Call Nothing fun [e1', e2']
resolveExp c@(S.ExpNE e1 e2) = do
  e1' <- resolve e1 `wrapError` c
  e2' <- resolve e2 `wrapError` c
  pure $ Call Nothing (Name "ne") [e1', e2']
resolveExp c@(S.ExpLAnd e1 e2) = do
  e1' <- resolve e1 `wrapError` c
  e2' <- resolve e2 `wrapError` c
  pure $ Call Nothing (Name "and") [e1', e2']
resolveExp c@(S.ExpLOr e1 e2) = do
  e1' <- resolve e1 `wrapError` c
  e2' <- resolve e2 `wrapError` c
  pure $ Call Nothing (Name "or") [e1', e2']
resolveExp c@(S.ExpLNot e) = do
  e' <- resolve e `wrapError` c
  pure $ Call Nothing (Name "not") [e']
resolveExp (S.ExpCond e1 e2 e3) =
  Cond <$> resolve e1 <*> resolve e2 <*> resolve e3
resolveExp (S.ExpAt t) = do
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
    locatedLike tc locatedTy <$> do
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
          (Name "integer", TTyCon),
          (Name "()", TTyCon),
          (Name "->", TTyCon),
          (Name "pair", TTyCon),
          (Name "sum", TTyCon)
        ]
    )
    (Map.fromList [(Name "invokable", TClass), (Name "Int", TClass)])
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
          (Name "primEqWord", TFunction),
          (Name "wordToInteger", TFunction),
          (Name "wordFromInteger", TFunction),
          (Name "integerAdd", TFunction),
          (Name "integerSub", TFunction),
          (Name "integerMul", TFunction),
          (Name "integerLt", TFunction),
          (Name "integerEq", TFunction),
          (QualName (Name "Int") "fromInteger", TFunction)
        ]
    )

globalEnv :: [S.TopDecl] -> Env
globalEnv = foldr addTopDecl emptyEnv

addImportsToEnv :: [S.Import] -> Env -> Env
addImportsToEnv imps env = foldr addImport env imps

addImport :: S.Import -> Env -> Env
addImport (S.ImportModule path) env =
  addModuleName (defaultImportModuleName path) env
addImport (S.ImportAlias _ n) env =
  addModuleName n env
addImport (S.ImportOnly _ _) env =
  env

defaultImportModuleName :: S.ModulePath -> Name
defaultImportModuleName (S.RelativePath path) = moduleLeafName path
defaultImportModuleName (S.LibraryPath path) = moduleLeafName path
defaultImportModuleName (S.ExternalPath _ path) = moduleLeafName path

modulePrefixes :: Name -> [Name]
modulePrefixes n =
  reverse (go n)
  where
    go q@(QualName p _) = q : go p
    go x = [x]

moduleLeafName :: Name -> Name
moduleLeafName (Name n) = Name n
moduleLeafName (QualName _ n) = Name n

addTopDecl :: S.TopDecl -> Env -> Env
addTopDecl (S.TContr (S.Contract n _ _)) env =
  addQualifiedModules n $
    env {typeEnv = Map.insert n TContract (typeEnv env)}
addTopDecl (S.TFunDef (S.FunDef _ sig _)) env =
  addQualifiedModules (S.sigName sig) $
    env {scopeEnv = Map.insert (S.sigName sig) TFunction (scopeEnv env)}
addTopDecl (S.TClassDef (S.Class _ _ n _ _ sigs)) env =
  let env' =
        foldr
          ( \s ac ->
              let qn = qualifyName n (S.sigName s)
               in Map.insert qn TFunction ac
          )
          (scopeEnv env)
          sigs
   in addQualifiedModules n $
        env
          { classEnv = Map.insert n TClass (classEnv env),
            scopeEnv = env'
          }
addTopDecl (S.TDataDef (S.DataTy n _ cons)) env =
  addQualifiedModules n $
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
  addQualifiedModules n $
    env {typeEnv = Map.insert n TTyCon (typeEnv env)}
addTopDecl (S.TExportDecl _) env = env
addTopDecl _ env = env

addModuleName :: Name -> Env -> Env
addModuleName n env =
  env {scopeEnv = Map.insertWith (\_ old -> old) n TModule (scopeEnv env)}

addQualifiedModules :: Name -> Env -> Env
addQualifiedModules (QualName qualifier _) env =
  foldr addModuleName env (modulePrefixes qualifier)
addQualifiedModules _ env = env

-- definition of a monad for name resolution

type ResolveM a = StateT Env (ExceptT CompilerError IO) a

runResolveM :: ResolveM a -> Env -> IO (Either CompilerError a)
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

-- For UFCS-style method calls (`value.method(args)`): find a class that has
-- a method named `m` so we can rewrite the call as `Class.method(value,args)`.
-- Returns the first match; ambiguity across multiple classes falls back to
-- the regular undefined-name path.
findClassWithMethod :: Name -> ResolveM (Maybe Name)
findClassWithMethod m =
  do
    env <- get
    let classes = Map.keys (classEnv env)
        matches =
          [ c
          | c <- classes,
            Map.lookup (QualName c (pretty m)) (scopeEnv env) == Just TFunction
          ]
    pure $ case matches of
      [c] -> Just c
      _ -> Nothing

wrapError :: (Pretty b, Data b) => ResolveM a -> b -> ResolveM a
wrapError m e =
  catchError m handler
  where
    handler msg = throwError (decorate msg)
    decorate (CompilerDiagnostics diagnostics) =
      CompilerDiagnostics $
        map
          (addDiagnosticNote ("in: " ++ pretty e) . addContextLabel e)
          diagnostics
    decorate (CompilerLegacyError msg) =
      CompilerLegacyError (msg ++ "\n - in:" ++ pretty e)

addContextLabel :: (Data b) => b -> Diagnostic -> Diagnostic
addContextLabel context diagnostic
  | any ((== Primary) . labelStyle) (diagnosticLabels diagnostic) = diagnostic
  | otherwise =
      case contextSourceSpan context of
        Just sourceSpan ->
          diagnostic
            { diagnosticLabels =
                Label
                  { labelSpan = sourceSpan,
                    labelStyle = Primary,
                    labelMessage = Just (contextLabelMessage diagnostic)
                  }
                  : diagnosticLabels diagnostic
            }
        Nothing -> diagnostic

contextLabelMessage :: Diagnostic -> String
contextLabelMessage diagnostic =
  case diagnosticCode diagnostic of
    Just (DiagnosticCode "SC0101") -> "unknown name"
    Just (DiagnosticCode "SC0102") -> "undefined type variable"
    Just (DiagnosticCode "SC0103") -> "undefined type constructor"
    Just (DiagnosticCode "SC0104") -> "invalid type synonym"
    Just (DiagnosticCode "SC0105") -> "undefined class"
    Just (DiagnosticCode "SC0106") -> "unqualified constructor"
    Just (DiagnosticCode "SC0107") -> "invalid pattern"
    _ -> "diagnostic reported here"

contextSourceSpan :: (Data a) => a -> Maybe SourceSpan
contextSourceSpan value =
  getFirst $ everything (<>) (mkQ (First Nothing) locationSpan `extQ` nameSpan) value
  where
    locationSpan :: NodeLocation -> First SourceSpan
    locationSpan = First . nodeLocationSpan

    nameSpan :: Name -> First SourceSpan
    nameSpan = First . nameSourceSpan

addContractName :: Name -> ResolveM ()
addContractName n =
  modify (\env -> env {typeEnv = Map.insert n TContract (typeEnv env)})

addFunctionName :: Name -> ResolveM ()
addFunctionName n = do
  existing <- gets (Map.lookup n . scopeEnv)
  case existing of
    Just TDataCon | isPrimitiveConstructor n -> pure ()
    _ -> modify (\env -> env {scopeEnv = Map.insert n TFunction (scopeEnv env)})

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
addDataCon typeName conName =
  modify
    ( \env ->
        env
          { scopeEnv =
              Map.insert (qualifiedConstructorName typeName conName) TDataCon (scopeEnv env)
          }
    )

addTyVar :: Name -> ResolveM ()
addTyVar n =
  modify (\env -> env {typeEnv = Map.insert n TTyVar (typeEnv env)})

resolveQualifiedConstructorName :: Name -> Name -> ResolveM Name
resolveQualifiedConstructorName qualifier conName =
  do
    let qn = qualifyName qualifier conName
    dt <- lookupName qn
    case dt of
      Just TDataCon -> pure qn
      _ ->
        let fallback = qualifyName (constructorLeafName qualifier) conName
         in do
              fdt <- lookupName fallback
              case fdt of
                Just TDataCon -> pure fallback
                _ -> undefinedName qn

-- error messages

undefinedTypeVariables :: [Name] -> ResolveM a
undefinedTypeVariables ns =
  diagnosticErrorWithLabels
    "SC0102"
    ("undefined type variables: " ++ unwords (map pretty ns))
    (mapMaybe (primaryNameLabel "undefined type variable") ns)
    []
    []

undefinedTypeConstructor :: S.Ty -> ResolveM a
undefinedTypeConstructor t =
  diagnosticErrorAtName
    "SC0103"
    ("undefined type constructor: " ++ pretty t)
    (S.tyName t)
    "undefined type constructor"
    []
    []

invalidTypeSynonymError :: S.TySym -> ResolveM a
invalidTypeSynonymError t =
  diagnosticErrorAtName
    "SC0104"
    ("invalid type synonym: " ++ pretty t)
    (S.symName t)
    "invalid type synonym"
    []
    []

undefinedClassError :: Name -> ResolveM a
undefinedClassError n =
  diagnosticErrorAtName
    "SC0105"
    ("undefined class: " ++ pretty n)
    n
    "undefined class"
    []
    []

undefinedName :: Name -> ResolveM a
undefinedName n =
  diagnosticErrorAtName
    "SC0101"
    ("undefined name: " ++ pretty n)
    n
    "unknown name"
    []
    []

unqualifiedConstructorError :: Name -> ResolveM a
unqualifiedConstructorError n =
  diagnosticErrorAtName
    "SC0106"
    ("unqualified constructor: " ++ pretty n)
    n
    "constructor must be qualified"
    []
    ["use Type.Constructor form"]

invalidPatternSyntax :: S.Pat -> ResolveM a
invalidPatternSyntax p =
  diagnosticError
    "SC0107"
    ("invalid pattern syntax: " ++ pretty p)
    []
    []

diagnosticError :: String -> String -> [String] -> [String] -> ResolveM a
diagnosticError code message notes help =
  diagnosticErrorWithLabels code message [] notes help

diagnosticErrorAtName :: String -> String -> Name -> String -> [String] -> [String] -> ResolveM a
diagnosticErrorAtName code message identName label notes help =
  diagnosticErrorWithLabels code message (maybe [] pure (primaryNameLabel label identName)) notes help

diagnosticErrorWithLabels :: String -> String -> [Label] -> [String] -> [String] -> ResolveM a
diagnosticErrorWithLabels code message labels notes help =
  throwError $ diagnosticCompilerError $ diagnosticValue code message labels notes help

diagnosticValue :: String -> String -> [Label] -> [String] -> [String] -> Diagnostic
diagnosticValue code message labels notes help =
  Diagnostic
    { diagnosticSeverity = Error,
      diagnosticCode = Just (DiagnosticCode code),
      diagnosticMessage = message,
      diagnosticLabels = labels,
      diagnosticNotes = notes,
      diagnosticHelp = help
    }

primaryNameLabel :: String -> Name -> Maybe Label
primaryNameLabel message identName =
  do
    sourceSpan <- nameSourceSpan identName
    pure
      Label
        { labelSpan = sourceSpan,
          labelStyle = Primary,
          labelMessage = Just message
        }
