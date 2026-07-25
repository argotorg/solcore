{-# LANGUAGE PatternSynonyms #-}

module Solcore.Frontend.Syntax.Contract where

import Data.Generics (Data, Typeable)
import Data.List.NonEmpty
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty

-- compilation unit

data CompUnit a
  = CompUnit
  { imports :: [Import],
    contracts :: [TopDecl a]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data TopDecl a
  = TContr (Contract a)
  | TFunDef (FunDef a)
  | TClassDef (Class a)
  | TInstDef (Instance a)
  | TMutualDef [TopDecl a]
  | TDataDef DataTy
  | TSym TySym
  | TExportDecl Export
  | TPragmaDecl Pragma
  deriving (Eq, Ord, Show, Data, Typeable)

-- empty list in pragma: restriction on all class / instances

data PragmaType
  = NoCoverageCondition
  | NoPattersonCondition
  | NoBoundVariableCondition
  | NoGenericInstanceFor
  | SolidityPragma String
  | AbiCoderPragma String
  deriving (Eq, Ord, Show, Data, Typeable)

data PragmaStatus
  = Enabled
  | DisableAll
  | DisableFor (NonEmpty Name)
  deriving (Eq, Ord, Show, Data, Typeable)

data Pragma
  = Pragma
  { pragmaType :: PragmaType,
    pragmaStatus :: PragmaStatus
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data ModulePath
  = RelativePath Name
  | LibraryPath Name
  | ExternalPath Name Name
  deriving (Eq, Ord, Show, Data, Typeable)

data Export
  = ExportList [ExportSpec]
  | ExportModule ModulePath
  | ExportModuleAs ModulePath Name
  | ExportItemsFrom ModulePath ExportSelector
  deriving (Eq, Ord, Show, Data, Typeable)

data ConstructorSelector
  = SelectConstructors [Name]
  | SelectAllConstructors
  deriving (Eq, Ord, Show, Data, Typeable)

data ExportSpec
  = ExportName Name
  | ExportNameWithConstructors Name ConstructorSelector
  | ExportAll
  | ExportModuleAll ModulePath
  deriving (Eq, Ord, Show, Data, Typeable)

data ExportSelector
  = SelectExportItems [ExportSelectorEntry]
  deriving (Eq, Ord, Show, Data, Typeable)

data ExportSelectorEntry
  = SelectExportAllItems
  | SelectExportItem Name
  | SelectExportConstructors Name ConstructorSelector
  deriving (Eq, Ord, Show, Data, Typeable)

data Import
  = ImportModule {importModule :: ModulePath}
  | ImportAlias {importModule :: ModulePath, importAlias :: Name}
  | ImportOnly {importModule :: ModulePath, importItems :: ItemSelector}
  deriving (Eq, Ord, Show, Data, Typeable)

data ItemSelector
  = SelectItems [ItemSelectorEntry] [Name]
  deriving (Eq, Ord, Show, Data, Typeable)

data ItemSelectorEntry
  = SelectAllItems
  | SelectItem Name
  | SelectItemAs Name Name
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of the contract structure

data ContractKind
  = ContractKind
  | InterfaceKind
  | LibraryKind
  deriving (Eq, Ord, Show, Data, Typeable)

data Contract a
  = ContractWithKind
  { contractKind :: ContractKind,
    name :: Name,
    tyParams :: [Tyvar],
    decls :: [ContractDecl a]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- Keep the historical three-argument semantic-AST constructor available to
-- external callers. Compiler passes use 'ContractWithKind' explicitly whenever
-- they reconstruct a declaration, so a preserved interface or library kind
-- cannot silently become an ordinary contract.
pattern Contract :: Name -> [Tyvar] -> [ContractDecl a] -> Contract a
pattern Contract n ts ds <- ContractWithKind _ n ts ds
  where
    Contract n ts ds = ContractWithKind ContractKind n ts ds

pattern ContractShell :: ContractKind -> Name -> [Tyvar] -> [ContractDecl a] -> Contract a
pattern ContractShell k n ts ds = ContractWithKind k n ts ds

{-# COMPLETE Contract #-}

{-# COMPLETE ContractShell #-}

-- definition of a algebraic data type

data DataTy
  = DataTyWithKind
  { dataTyKind :: DataTyKind,
    dataName :: Name,
    dataParams :: [Tyvar],
    dataConstrs :: [Constr]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data DataTyKind
  = EnumKind
  | StructKind [Name]
  deriving (Eq, Ord, Show, Data, Typeable)

-- Keep the historical three-argument constructor available to compiler passes.
-- Synthetic declarations are enums by default, while declarations originating
-- from source use 'DataTyWithKind' to retain their exact syntax kind.
pattern DataTy :: Name -> [Tyvar] -> [Constr] -> DataTy
pattern DataTy n ts cs <- DataTyWithKind _ n ts cs
  where
    DataTy n ts cs = DataTyWithKind EnumKind n ts cs

pattern StructTy :: Name -> [Tyvar] -> [Name] -> [Ty] -> DataTy
pattern StructTy n ts fieldNames fieldTypes <-
  DataTyWithKind (StructKind fieldNames) n ts [Constr _ fieldTypes]
  where
    StructTy n ts fieldNames fieldTypes =
      DataTyWithKind (StructKind fieldNames) n ts [Constr n fieldTypes]

{-# COMPLETE DataTy #-}

data Constr
  = Constr
  { constrName :: Name,
    constrTy :: [Ty]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of type synonym

data TySym
  = TySym
  { symName :: Name,
    symVars :: [Tyvar],
    symType :: Ty
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of contract constructor

data Constructor a
  = Constructor
  { constrParams :: [Param a],
    constrBody :: (Body a),
    constrPayable :: Bool
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of classes and instances

data Class a
  = Class
  { classboundvars :: [Tyvar],
    classContext :: [Pred],
    className :: Name,
    paramsVar :: [Tyvar],
    mainVar :: Tyvar,
    signatures :: [Signature a]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- | One source-level item in a function's @returns (...)@ clause.
--
-- The aggregate 'sigReturn' type deliberately remains available because the
-- type checker and backend operate on a single result type.  This metadata
-- preserves the item boundaries that aggregation loses: in particular,
-- @returns (result: (word, bool))@ is one tuple-valued ABI output, whereas
-- @returns (word, bool)@ is two outputs.
data SignatureReturnItem
  = SignatureReturnItem
  { signatureReturnItemComptime :: Bool,
    signatureReturnItemName :: Maybe Name,
    signatureReturnItemType :: Ty
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Solidity-style source visibility retained after name resolution.
--
-- 'Nothing' in 'sigVisibility' is meaningful: module-level functions and
-- compiler-generated functions do not carry a contract visibility modifier.
data FunctionVisibility
  = VisibilityPublic
  | VisibilityExternal
  | VisibilityInternal
  | VisibilityPrivate
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Solidity-style state mutability retained after name resolution.
--
-- A signature with no mutability modifier is nonpayable. Keeping the source
-- modifier itself lets ABI generation distinguish @pure@ and @view@ from that
-- default, instead of collapsing all three to a legacy payability bit.
data FunctionMutability
  = MutabilityPure
  | MutabilityView
  | MutabilityPayable
  deriving (Eq, Ord, Show, Data, Typeable)

data FunctionModifier
  = VisibilityModifier FunctionVisibility
  | MutabilityModifier FunctionMutability
  deriving (Eq, Ord, Show, Data, Typeable)

data Signature a
  = SignatureWithReturnNames
  { sigVars :: [Tyvar],
    sigContext :: [Pred],
    sigName :: Name,
    sigParams :: [Param a],
    sigRetComptime :: Bool,
    sigReturn :: Maybe Ty,
    sigPayable :: Bool,
    -- | Solidity ABI names for return values. An empty list is the compact
    -- representation used by legacy/compiler-generated signatures whose
    -- returns are all unnamed.
    sigReturnNames :: [Maybe Name],
    -- | Exact source return-item boundaries and per-item comptime metadata.
    -- Legacy and compiler-generated signatures leave this empty and continue
    -- to use 'sigReturn', 'sigRetComptime', and 'sigReturnNames'.
    sigReturnItems :: [SignatureReturnItem],
    -- | Exact contract visibility and mutability modifiers from the source.
    -- Legacy/compiler-generated signatures leave this empty.
    sigModifiers :: [FunctionModifier]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- Keep the historical seven-argument constructor available throughout the
-- compiler. Source name resolution uses the richer constructor for explicit
-- return clauses, while generated and legacy signatures can stay compact.
pattern Signature ::
  [Tyvar] ->
  [Pred] ->
  Name ->
  [Param a] ->
  Bool ->
  Maybe Ty ->
  Bool ->
  Signature a
pattern Signature vars context funName params returnComptime returnTy payable <-
  SignatureWithReturnNames
    vars
    context
    funName
    params
    returnComptime
    returnTy
    payable
    _
    _
    _
  where
    Signature vars context funName params returnComptime returnTy payable =
      SignatureWithReturnNames
        vars
        context
        funName
        params
        returnComptime
        returnTy
        payable
        []
        []
        [MutabilityModifier MutabilityPayable | payable]

{-# COMPLETE Signature #-}

sigVisibility :: Signature a -> Maybe FunctionVisibility
sigVisibility sig =
  case [visibility | VisibilityModifier visibility <- sigModifiers sig] of
    visibility : _ -> Just visibility
    [] -> Nothing

sigMutability :: Signature a -> Maybe FunctionMutability
sigMutability sig =
  case [mutability | MutabilityModifier mutability <- sigModifiers sig] of
    mutability : _ -> Just mutability
    [] -> Nothing

data Instance a
  = Instance
  { instDefault :: Bool,
    instVars :: [Tyvar],
    instContext :: [Pred],
    instName :: Name,
    paramsTy :: [Ty],
    mainTy :: Ty,
    instFunctions :: [FunDef a]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

instanceHeadKey :: Instance a -> (Bool, Name, [Ty], Ty)
instanceHeadKey inst =
  (instDefault inst, instName inst, paramsTy inst, mainTy inst)

data TopDeclKey
  = ContractKey Name
  | FunKey Name
  | ClassKey Name
  | InstanceKey (Bool, Name, [Ty], Ty)
  | DataKey Name
  | SynonymKey Name
  deriving (Eq, Ord, Show, Data, Typeable)

topDeclKeys :: TopDecl a -> [TopDeclKey]
topDeclKeys (TContr contractDef) = [ContractKey (name contractDef)]
topDeclKeys (TFunDef funDef) = [FunKey (sigName (funSignature funDef))]
topDeclKeys (TClassDef cls) = [ClassKey (className cls)]
topDeclKeys (TInstDef inst) = [InstanceKey (instanceHeadKey inst)]
topDeclKeys (TMutualDef mutualDecls) = mutualDecls >>= topDeclKeys
topDeclKeys (TDataDef dataTy) = [DataKey (dataName dataTy)]
topDeclKeys (TSym tySym) = [SynonymKey (symName tySym)]
topDeclKeys (TExportDecl _) = []
topDeclKeys (TPragmaDecl _) = []

-- definition of contract field variables

data Field a
  = Field
  { fieldName :: Name,
    fieldTy :: Ty,
    fieldInit :: Maybe (Exp a)
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of functions

data FunDef a
  = FunDef
  { funIsPublic :: Bool,
    funSignature :: Signature a,
    funDefBody :: Body a
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data ContractDecl a
  = CDataDecl DataTy
  | CFieldDecl (Field a)
  | CFunDecl (FunDef a)
  | CSignatureDecl Bool (Signature a)
  | CMutualDecl [ContractDecl a] -- used only after SCC analysis
  | CConstrDecl (Constructor a)
  deriving (Eq, Ord, Show, Data, Typeable)

instance (HasSourceSpan a) => HasSourceSpan (CompUnit a) where
  sourceSpanOf (CompUnit imps ds) =
    firstSourceSpan [sourceSpanOf imps, sourceSpanOf ds]

instance (HasSourceSpan a) => HasSourceSpan (TopDecl a) where
  sourceSpanOf (TContr contractDef) = sourceSpanOf contractDef
  sourceSpanOf (TFunDef funDef) = sourceSpanOf funDef
  sourceSpanOf (TClassDef cls) = sourceSpanOf cls
  sourceSpanOf (TInstDef inst) = sourceSpanOf inst
  sourceSpanOf (TMutualDef mutualDecls) = sourceSpanOf mutualDecls
  sourceSpanOf (TDataDef dataTy) = sourceSpanOf dataTy
  sourceSpanOf (TSym tySym) = sourceSpanOf tySym
  sourceSpanOf (TExportDecl exportDecl) = sourceSpanOf exportDecl
  sourceSpanOf (TPragmaDecl pragma) = sourceSpanOf pragma

instance HasSourceSpan Pragma where
  sourceSpanOf (Pragma _ status) = sourceSpanOf status

instance HasSourceSpan PragmaStatus where
  sourceSpanOf Enabled = Nothing
  sourceSpanOf DisableAll = Nothing
  sourceSpanOf (DisableFor names) = sourceSpanOf (toList names)

instance HasSourceSpan ModulePath where
  sourceSpanOf (RelativePath n) = sourceSpanOf n
  sourceSpanOf (LibraryPath n) = sourceSpanOf n
  sourceSpanOf (ExternalPath libName modName) =
    combineMaybeSourceSpans (sourceSpanOf libName) (sourceSpanOf modName)

instance HasSourceSpan Export where
  sourceSpanOf (ExportList specs) = sourceSpanOf specs
  sourceSpanOf (ExportModule modulePath) = sourceSpanOf modulePath
  sourceSpanOf (ExportModuleAs modulePath aliasName) =
    firstSourceSpan [sourceSpanOf modulePath, sourceSpanOf aliasName]
  sourceSpanOf (ExportItemsFrom modulePath selector) =
    firstSourceSpan [sourceSpanOf modulePath, sourceSpanOf selector]

instance HasSourceSpan ExportSpec where
  sourceSpanOf (ExportName n) = sourceSpanOf n
  sourceSpanOf (ExportNameWithConstructors typeName selector) =
    firstSourceSpan [sourceSpanOf typeName, sourceSpanOf selector]
  sourceSpanOf ExportAll = Nothing
  sourceSpanOf (ExportModuleAll modulePath) = sourceSpanOf modulePath

instance HasSourceSpan ConstructorSelector where
  sourceSpanOf (SelectConstructors names) = sourceSpanOf names
  sourceSpanOf SelectAllConstructors = Nothing

instance HasSourceSpan ExportSelector where
  sourceSpanOf (SelectExportItems items) = sourceSpanOf items

instance HasSourceSpan ExportSelectorEntry where
  sourceSpanOf SelectExportAllItems = Nothing
  sourceSpanOf (SelectExportItem n) = sourceSpanOf n
  sourceSpanOf (SelectExportConstructors typeName selector) =
    firstSourceSpan [sourceSpanOf typeName, sourceSpanOf selector]

instance HasSourceSpan Import where
  sourceSpanOf (ImportModule modulePath) = sourceSpanOf modulePath
  sourceSpanOf (ImportAlias modulePath aliasName) =
    firstSourceSpan [sourceSpanOf modulePath, sourceSpanOf aliasName]
  sourceSpanOf (ImportOnly modulePath items) =
    firstSourceSpan [sourceSpanOf modulePath, sourceSpanOf items]

instance HasSourceSpan ItemSelector where
  sourceSpanOf (SelectItems items hidden) =
    firstSourceSpan [sourceSpanOf items, sourceSpanOf hidden]

instance HasSourceSpan ItemSelectorEntry where
  sourceSpanOf SelectAllItems = Nothing
  sourceSpanOf (SelectItem n) = sourceSpanOf n
  sourceSpanOf (SelectItemAs n aliasName) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf aliasName]

instance (HasSourceSpan a) => HasSourceSpan (Contract a) where
  sourceSpanOf (ContractWithKind _ n tyVars contractDecls) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf tyVars, sourceSpanOf contractDecls]

instance HasSourceSpan DataTy where
  sourceSpanOf (DataTyWithKind kind n tyVars constrs) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf tyVars, sourceSpanOf kind, sourceSpanOf constrs]

instance HasSourceSpan DataTyKind where
  sourceSpanOf EnumKind = Nothing
  sourceSpanOf (StructKind fieldNames) = sourceSpanOf fieldNames

instance HasSourceSpan Constr where
  sourceSpanOf (Constr n tys) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf tys]

instance HasSourceSpan TySym where
  sourceSpanOf (TySym n tyVars ty) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf tyVars, sourceSpanOf ty]

instance (HasSourceSpan a) => HasSourceSpan (Constructor a) where
  sourceSpanOf (Constructor params body _) =
    firstSourceSpan [sourceSpanOf params, sourceSpanOf body]

instance (HasSourceSpan a) => HasSourceSpan (Class a) where
  sourceSpanOf (Class boundVars context clsName params main signatures') =
    firstSourceSpan [sourceSpanOf boundVars, sourceSpanOf context, sourceSpanOf clsName, sourceSpanOf params, sourceSpanOf main, sourceSpanOf signatures']

instance (HasSourceSpan a) => HasSourceSpan (Signature a) where
  sourceSpanOf (Signature vars context sig params _ returnTy _) =
    firstSourceSpan [sourceSpanOf vars, sourceSpanOf context, sourceSpanOf sig, sourceSpanOf params, sourceSpanOf returnTy]

instance (HasSourceSpan a) => HasSourceSpan (Instance a) where
  sourceSpanOf (Instance _ vars context clsName params main funs) =
    firstSourceSpan [sourceSpanOf vars, sourceSpanOf context, sourceSpanOf clsName, sourceSpanOf params, sourceSpanOf main, sourceSpanOf funs]

instance (HasSourceSpan a) => HasSourceSpan (Field a) where
  sourceSpanOf (Field n ty initExp) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf ty, sourceSpanOf initExp]

instance (HasSourceSpan a) => HasSourceSpan (FunDef a) where
  sourceSpanOf (FunDef _ sig body) =
    firstSourceSpan [sourceSpanOf sig, sourceSpanOf body]

instance (HasSourceSpan a) => HasSourceSpan (ContractDecl a) where
  sourceSpanOf (CDataDecl dataTy) = sourceSpanOf dataTy
  sourceSpanOf (CFieldDecl field) = sourceSpanOf field
  sourceSpanOf (CFunDecl funDef) = sourceSpanOf funDef
  sourceSpanOf (CSignatureDecl _ sig) = sourceSpanOf sig
  sourceSpanOf (CMutualDecl decls') = sourceSpanOf decls'
  sourceSpanOf (CConstrDecl constructor) = sourceSpanOf constructor
