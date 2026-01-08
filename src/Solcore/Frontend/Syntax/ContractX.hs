{-# LANGUAGE GHC2021, DataKinds, GADTs, StandaloneDeriving, ConstraintKinds, UndecidableInstances, DeriveDataTypeable #-}
module Solcore.Frontend.Syntax.ContractX where

import GHC.Types
import Data.Generics (Data, Typeable)
import Data.List.NonEmpty

import Solcore.Frontend.Syntax.StmtX
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.Syntax.Name

-- ============================================================================
-- Extension families for TopDeclX
-- ============================================================================

type family XTContr x
type family XTFunDef x
type family XTClassDef x
type family XTInstDef x
type family XTMutualDef x
type family XTDataDef x
type family XTSym x
type family XTPragmaDecl x
type family XXTopDecl x

type instance XTContr ComPs = NoExtField
type instance XTContr ComNm = NoExtField
type instance XTContr ComTc = NoExtField
type instance XTContr ComSp = NoExtField

type instance XTFunDef ComPs = NoExtField
type instance XTFunDef ComNm = NoExtField
type instance XTFunDef ComTc = NoExtField
type instance XTFunDef ComSp = NoExtField

type instance XTClassDef ComPs = NoExtField
type instance XTClassDef ComNm = NoExtField
type instance XTClassDef ComTc = NoExtField
type instance XTClassDef ComSp = NoExtField

type instance XTInstDef ComPs = NoExtField
type instance XTInstDef ComNm = NoExtField
type instance XTInstDef ComTc = NoExtField
type instance XTInstDef ComSp = NoExtField

type instance XTMutualDef ComPs = NoExtField
type instance XTMutualDef ComNm = NoExtField
type instance XTMutualDef ComTc = NoExtField
type instance XTMutualDef ComSp = NoExtField

type instance XTDataDef ComPs = NoExtField
type instance XTDataDef ComNm = NoExtField
type instance XTDataDef ComTc = NoExtField
type instance XTDataDef ComSp = NoExtField

type instance XTSym ComPs = NoExtField
type instance XTSym ComNm = NoExtField
type instance XTSym ComTc = NoExtField
type instance XTSym ComSp = NoExtField

type instance XTPragmaDecl ComPs = NoExtField
type instance XTPragmaDecl ComNm = NoExtField
type instance XTPragmaDecl ComTc = NoExtField
type instance XTPragmaDecl ComSp = NoExtField

type instance XXTopDecl ComPs = DataConCantHappen
type instance XXTopDecl ComNm = DataConCantHappen
type instance XXTopDecl ComTc = DataConCantHappen
type instance XXTopDecl ComSp = DataConCantHappen

-- ============================================================================
-- Extension families for ContractDeclX
-- ============================================================================

type family XCDataDecl x
type family XCFieldDecl x
type family XCFunDecl x
type family XCMutualDecl x
type family XCConstrDecl x
type family XXContractDecl x

type instance XCDataDecl ComPs = NoExtField
type instance XCDataDecl ComNm = NoExtField
type instance XCDataDecl ComTc = NoExtField
type instance XCDataDecl ComSp = NoExtField

type instance XCFieldDecl ComPs = NoExtField
type instance XCFieldDecl ComNm = NoExtField
type instance XCFieldDecl ComTc = NoExtField
type instance XCFieldDecl ComSp = NoExtField

type instance XCFunDecl ComPs = NoExtField
type instance XCFunDecl ComNm = NoExtField
type instance XCFunDecl ComTc = NoExtField
type instance XCFunDecl ComSp = NoExtField

type instance XCMutualDecl ComPs = NoExtField
type instance XCMutualDecl ComNm = NoExtField
type instance XCMutualDecl ComTc = NoExtField
type instance XCMutualDecl ComSp = NoExtField

type instance XCConstrDecl ComPs = NoExtField
type instance XCConstrDecl ComNm = NoExtField
type instance XCConstrDecl ComTc = NoExtField
type instance XCConstrDecl ComSp = NoExtField

type instance XXContractDecl ComPs = DataConCantHappen
type instance XXContractDecl ComNm = DataConCantHappen
type instance XXContractDecl ComTc = DataConCantHappen
type instance XXContractDecl ComSp = DataConCantHappen

-- ============================================================================
-- Constraint helper type
-- ============================================================================

type ForallContractX (p :: Type -> Constraint) x =
    ( -- TopDeclX extensions
      p(XTContr x), p(XTFunDef x), p(XTClassDef x), p(XTInstDef x)
    , p(XTMutualDef x), p(XTDataDef x), p(XTSym x), p(XTPragmaDecl x)
    , p(XXTopDecl x)
      -- ContractDeclX extensions
    , p(XCDataDecl x), p(XCFieldDecl x), p(XCFunDecl x)
    , p(XCMutualDecl x), p(XCConstrDecl x), p(XXContractDecl x)
      -- From StmtX (needed for nested types)
    , ForallStmtX p x
    )

-- ============================================================================
-- Top-level compilation unit
-- ============================================================================

data CompUnitX x = CompUnitX {
    importsX :: [Import]
  , contractsX :: [TopDeclX x]
  }

deriving instance (ForallContractX Eq x) => Eq (CompUnitX x)
deriving instance (ForallContractX Ord x) => Ord (CompUnitX x)
deriving instance (ForallContractX Show x) => Show (CompUnitX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (CompUnitX x)

-- ============================================================================
-- Top-level declarations
-- ============================================================================

data TopDeclX x
  = TContrX (XTContr x) (ContractX x)
  | TFunDefX (XTFunDef x) (FunDefX x)
  | TClassDefX (XTClassDef x) (ClassX x)
  | TInstDefX (XTInstDef x) (InstanceX x)
  | TMutualDefX (XTMutualDef x) [TopDeclX x]
  | TDataDefX (XTDataDef x) DataTy
  | TSymX (XTSym x) TySym
  | TPragmaDeclX (XTPragmaDecl x) Pragma
  | XTopDeclX (XXTopDecl x)

deriving instance (ForallContractX Eq x) => Eq (TopDeclX x)
deriving instance (ForallContractX Ord x) => Ord (TopDeclX x)
deriving instance (ForallContractX Show x) => Show (TopDeclX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (TopDeclX x)

-- ============================================================================
-- Contract definition
-- ============================================================================

data ContractX x = ContractX {
    contractName :: Name
  , contractTyParams :: [Tyvar]
  , contractDecls :: [ContractDeclX x]
  }

deriving instance (ForallContractX Eq x) => Eq (ContractX x)
deriving instance (ForallContractX Ord x) => Ord (ContractX x)
deriving instance (ForallContractX Show x) => Show (ContractX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (ContractX x)

-- ============================================================================
-- Contract fields
-- ============================================================================

data FieldX x = FieldX {
    fieldNameX :: Name
  , fieldTyX :: Ty
  , fieldInitX :: Maybe (ExpX x)
  }

deriving instance (ForallContractX Eq x) => Eq (FieldX x)
deriving instance (ForallContractX Ord x) => Ord (FieldX x)
deriving instance (ForallContractX Show x) => Show (FieldX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (FieldX x)

-- ============================================================================
-- Contract constructor
-- ============================================================================

data ConstructorX x = ConstructorX {
    constrParamsX :: [ParamX x]
  , constrBodyX :: BodyX x
  }

deriving instance (ForallContractX Eq x) => Eq (ConstructorX x)
deriving instance (ForallContractX Ord x) => Ord (ConstructorX x)
deriving instance (ForallContractX Show x) => Show (ConstructorX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (ConstructorX x)

-- ============================================================================
-- Function definitions
-- ============================================================================

data FunDefX x = FunDefX {
    funSignatureX :: SignatureX x
  , funDefBodyX :: BodyX x
  }

deriving instance (ForallContractX Eq x) => Eq (FunDefX x)
deriving instance (ForallContractX Ord x) => Ord (FunDefX x)
deriving instance (ForallContractX Show x) => Show (FunDefX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (FunDefX x)

-- ============================================================================
-- Function signatures
-- ============================================================================

data SignatureX x = SignatureX {
    sigVarsX :: [Tyvar]
  , sigContextX :: [Pred]
  , sigNameX :: Name
  , sigParamsX :: [ParamX x]
  , sigReturnX :: Maybe Ty
  }

deriving instance (ForallContractX Eq x) => Eq (SignatureX x)
deriving instance (ForallContractX Ord x) => Ord (SignatureX x)
deriving instance (ForallContractX Show x) => Show (SignatureX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (SignatureX x)

-- ============================================================================
-- Classes
-- ============================================================================

data ClassX x = ClassX {
    classBoundVarsX :: [Tyvar]
  , classContextX :: [Pred]
  , classNameX :: Name
  , classParamsVarX :: [Tyvar]
  , classMainVarX :: Tyvar
  , classSignaturesX :: [SignatureX x]
  }

deriving instance (ForallContractX Eq x) => Eq (ClassX x)
deriving instance (ForallContractX Ord x) => Ord (ClassX x)
deriving instance (ForallContractX Show x) => Show (ClassX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (ClassX x)

-- ============================================================================
-- Instances
-- ============================================================================

data InstanceX x = InstanceX {
    instDefaultX :: Bool
  , instVarsX :: [Tyvar]
  , instContextX :: [Pred]
  , instNameX :: Name
  , instParamsTyX :: [Ty]
  , instMainTyX :: Ty
  , instFunctionsX :: [FunDefX x]
  }

deriving instance (ForallContractX Eq x) => Eq (InstanceX x)
deriving instance (ForallContractX Ord x) => Ord (InstanceX x)
deriving instance (ForallContractX Show x) => Show (InstanceX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (InstanceX x)

-- ============================================================================
-- Contract declarations
-- ============================================================================

data ContractDeclX x
  = CDataDeclX (XCDataDecl x) DataTy
  | CFieldDeclX (XCFieldDecl x) (FieldX x)
  | CFunDeclX (XCFunDecl x) (FunDefX x)
  | CMutualDeclX (XCMutualDecl x) [ContractDeclX x]
  | CConstrDeclX (XCConstrDecl x) (ConstructorX x)
  | XContractDeclX (XXContractDecl x)

deriving instance (ForallContractX Eq x) => Eq (ContractDeclX x)
deriving instance (ForallContractX Ord x) => Ord (ContractDeclX x)
deriving instance (ForallContractX Show x) => Show (ContractDeclX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (ContractDeclX x)

-- ============================================================================
-- Phase-independent types
-- ============================================================================

-- empty list in pragma: restriction on all class / instances

data PragmaType
  = NoCoverageCondition
  | NoPattersonCondition
  | NoBoundVariableCondition
  deriving (Eq, Ord, Show, Data, Typeable)

data PragmaStatus
  = Enabled
  | DisableAll
  | DisableFor (NonEmpty Name)
  deriving (Eq, Ord, Show, Data, Typeable)

data Pragma
  = Pragma {
      pragmaType :: PragmaType
    , pragmaStatus :: PragmaStatus
    } deriving (Eq, Ord, Show, Data, Typeable)

newtype Import
  = Import { unImport :: Name }
    deriving (Eq, Ord, Show, Data, Typeable)

data DataTy
  = DataTy {
      dataName :: Name
    , dataParams :: [Tyvar]
    , dataConstrs :: [Constr]
    } deriving (Eq, Ord, Show, Data, Typeable)

data Constr
  = Constr {
      constrName :: Name
    , constrTy :: [Ty]
    } deriving (Eq, Ord, Show, Data, Typeable)

-- definition of type synonym

data TySym
  = TySym {
      symName :: Name
    , symVars :: [Tyvar]
    , symType :: Ty
    } deriving (Eq, Ord, Show, Data, Typeable)



-- ============================================================================
-- Type synonyms for common passes
-- ============================================================================

type NamedCompUnit = CompUnitX ComNm
type NamedTopDecl = TopDeclX ComNm
type NamedContract = ContractX ComNm
type NamedField = FieldX ComNm
type NamedFunDef = FunDefX ComNm
type NamedClass = ClassX ComNm
type NamedInstance = InstanceX ComNm
type NamedSignature = SignatureX ComNm
type NamedConstructor = ConstructorX ComNm
type NamedContractDecl = ContractDeclX ComNm

-- ============================================================================
-- Pattern synonyms for migration ease
-- ============================================================================

-- TopDecl patterns
pattern TContr :: ContractX ComNm -> TopDeclX ComNm
pattern TContr c = TContrX NoExtField c

pattern TFunDef :: FunDefX ComNm -> TopDeclX ComNm
pattern TFunDef f = TFunDefX NoExtField f

pattern TClassDef :: ClassX ComNm -> TopDeclX ComNm
pattern TClassDef cls = TClassDefX NoExtField cls

pattern TInstDef :: InstanceX ComNm -> TopDeclX ComNm
pattern TInstDef inst = TInstDefX NoExtField inst

pattern TMutualDef :: [TopDeclX ComNm] -> TopDeclX ComNm
pattern TMutualDef decls = TMutualDefX NoExtField decls

pattern TDataDef :: DataTy -> TopDeclX ComNm
pattern TDataDef dt = TDataDefX NoExtField dt

pattern TSym :: TySym -> TopDeclX ComNm
pattern TSym ts = TSymX NoExtField ts

pattern TPragmaDecl :: Pragma -> TopDeclX ComNm
pattern TPragmaDecl p = TPragmaDeclX NoExtField p

{-# COMPLETE TContr, TFunDef, TClassDef, TInstDef, TMutualDef, TDataDef, TSym, TPragmaDecl, XTopDeclX #-}

-- ContractDecl patterns
pattern CDataDecl :: DataTy -> ContractDeclX ComNm
pattern CDataDecl dt = CDataDeclX NoExtField dt

pattern CFieldDecl :: FieldX ComNm -> ContractDeclX ComNm
pattern CFieldDecl f = CFieldDeclX NoExtField f

pattern CFunDecl :: FunDefX ComNm -> ContractDeclX ComNm
pattern CFunDecl fd = CFunDeclX NoExtField fd

pattern CMutualDecl :: [ContractDeclX ComNm] -> ContractDeclX ComNm
pattern CMutualDecl decls = CMutualDeclX NoExtField decls

pattern CConstrDecl :: ConstructorX ComNm -> ContractDeclX ComNm
pattern CConstrDecl c = CConstrDeclX NoExtField c

{-# COMPLETE CDataDecl, CFieldDecl, CFunDecl, CMutualDecl, CConstrDecl, XContractDeclX #-}
