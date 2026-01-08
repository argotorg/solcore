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

type family XCompUnit x
type family XConstructor x
type family XContract x
type family XTContr x
type family XTFunDef x
type family XTClassDef x
type family XTInstDef x
type family XTMutualDef x
type family XTDataDef x
type family XTSym x
type family XTPragmaDecl x
type family XXTopDecl x

type instance XCompUnit ComPs = NoExtField
type instance XCompUnit ComNm = NoExtField
type instance XCompUnit ComTc = NoExtField
type instance XCompUnit ComSp = NoExtField

type instance XConstructor ComPs = NoExtField
type instance XConstructor ComNm = NoExtField
type instance XConstructor ComTc = NoExtField
type instance XConstructor ComSp = NoExtField

type instance XContract ComPs = NoExtField
type instance XContract ComNm = NoExtField
type instance XContract ComTc = NoExtField
type instance XContract ComSp = NoExtField

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
-- Extension families for record types
-- ============================================================================

type family XField x
type family XClass x
type family XSignature x
type family XInstance x

type instance XField ComPs = NoExtField
type instance XField ComNm = NoExtField
type instance XField ComTc = NoExtField
type instance XField ComSp = NoExtField

type instance XClass ComPs = NoExtField
type instance XClass ComNm = NoExtField
type instance XClass ComTc = NoExtField
type instance XClass ComSp = NoExtField

type instance XSignature ComPs = NoExtField
type instance XSignature ComNm = NoExtField
type instance XSignature ComTc = NoExtField
type instance XSignature ComSp = NoExtField

type instance XInstance ComPs = NoExtField
type instance XInstance ComNm = NoExtField
type instance XInstance ComTc = NoExtField
type instance XInstance ComSp = NoExtField

-- ============================================================================
-- Constraint helper type
-- ============================================================================

type ForallContractX (p :: Type -> Constraint) x =
    ( p(XField x), p(XClass x), p(XSignature x), p(XInstance x)
    , p(XConstructor x)
    , p(XContract x)
    , p(XCompUnit x)
      -- TopDeclX extensions
    , p(XTContr x), p(XTFunDef x), p(XTClassDef x), p(XTInstDef x)
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
    cuExtension :: XCompUnit x
  , importsX :: [Import]
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
    contractExtension :: XContract x
  , contractName :: Name
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
    fieldExtension :: XField x
  , fieldNameX :: Name
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
    constrExtension :: XConstructor x
  , constrParamsX :: [ParamX x]
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
    sigExtension :: XSignature x
  , sigVarsX :: [Tyvar]
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
    classExtension :: XClass x
  , classBoundVarsX :: [Tyvar]
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
    instExtension :: XInstance x
  , instDefaultX :: Bool
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


pattern CompUnit :: [Import] -> [TopDeclX ComNm] -> CompUnitX ComNm
pattern CompUnit is ds <- CompUnitX _ is ds
    where CompUnit is ds = CompUnitX mempty is ds

pattern Contract :: Name -> [Tyvar] -> [NamedContractDecl] -> NamedContract
pattern Contract n vs ds <- ContractX _  n vs ds
    where Contract n vs ds = ContractX mempty n vs ds

-- TopDecl patterns
pattern TContr :: ContractX ComNm -> TopDeclX ComNm
pattern TContr c <- TContrX _ c
    where TContr c = TContrX NoExtField c

pattern TFunDef :: FunDefX ComNm -> TopDeclX ComNm
pattern TFunDef f <- TFunDefX _ f
    where TFunDef f = TFunDefX NoExtField f

pattern TClassDef :: ClassX ComNm -> TopDeclX ComNm
pattern TClassDef cls <- TClassDefX _ cls
    where TClassDef cls = TClassDefX NoExtField cls

pattern TInstDef :: InstanceX ComNm -> TopDeclX ComNm
pattern TInstDef inst <- TInstDefX _ inst
    where TInstDef inst = TInstDefX NoExtField inst

pattern TMutualDef :: [TopDeclX ComNm] -> TopDeclX ComNm
pattern TMutualDef decls <- TMutualDefX _ decls
    where TMutualDef decls = TMutualDefX NoExtField decls

pattern TDataDef :: DataTy -> TopDeclX ComNm
pattern TDataDef dt <- TDataDefX _ dt
    where TDataDef dt = TDataDefX NoExtField dt

pattern TSym :: TySym -> TopDeclX ComNm
pattern TSym ts <- TSymX _ ts
    where TSym ts = TSymX NoExtField ts

pattern TPragmaDecl :: Pragma -> TopDeclX ComNm
pattern TPragmaDecl p <- TPragmaDeclX _ p
    where TPragmaDecl p = TPragmaDeclX NoExtField p

{-# COMPLETE TContr, TFunDef, TClassDef, TInstDef, TMutualDef, TDataDef, TSym, TPragmaDecl #-}

-- ContractDecl patterns
pattern CDataDecl :: DataTy -> ContractDeclX ComNm
pattern CDataDecl dt <- CDataDeclX _ dt
    where CDataDecl dt = CDataDeclX NoExtField dt

pattern CFieldDecl :: FieldX ComNm -> ContractDeclX ComNm
pattern CFieldDecl f <- CFieldDeclX _ f
    where CFieldDecl f = CFieldDeclX NoExtField f

pattern CFunDecl :: FunDefX ComNm -> ContractDeclX ComNm
pattern CFunDecl fd <- CFunDeclX _ fd
    where CFunDecl fd = CFunDeclX NoExtField fd

pattern CMutualDecl :: [ContractDeclX ComNm] -> ContractDeclX ComNm
pattern CMutualDecl decls <- CMutualDeclX _ decls
    where CMutualDecl decls = CMutualDeclX NoExtField decls

pattern CConstrDecl :: ConstructorX ComNm -> ContractDeclX ComNm
pattern CConstrDecl c <- CConstrDeclX _ c
    where CConstrDecl c = CConstrDeclX NoExtField c

{-# COMPLETE CDataDecl, CFieldDecl, CFunDecl, CMutualDecl, CConstrDecl #-}

pattern Constructor :: [NamedParam] -> NamedBody -> NamedConstructor
pattern Constructor ps b <- ConstructorX _ ps b where
    Constructor ps b = ConstructorX mempty ps b

-- Field patterns
pattern Field :: Name -> Ty -> Maybe NamedExp -> NamedField
pattern Field fn fty fin <- FieldX _ fn fty fin
    where Field fn fty fin = FieldX mempty fn fty fin

-- Class patterns
pattern Class :: [Tyvar] -> [Pred] -> Name -> [Tyvar] -> Tyvar -> [NamedSignature] -> NamedClass
pattern Class bvs ctx cn pvs mv sigs <- ClassX _ bvs ctx cn pvs mv sigs
    where Class bvs ctx cn pvs mv sigs = ClassX mempty bvs ctx cn pvs mv sigs

-- Signature patterns
pattern Signature :: [Tyvar] -> [Pred] -> Name -> [NamedParam] -> Maybe Ty -> NamedSignature
pattern Signature svs sctx sn sps sret <- SignatureX _ svs sctx sn sps sret
    where Signature svs sctx sn sps sret = SignatureX mempty svs sctx sn sps sret

-- Instance patterns
pattern Instance :: Bool -> [Tyvar] -> [Pred] -> Name -> [Ty] -> Ty -> [NamedFunDef] -> NamedInstance
pattern Instance def ivs ictx inn ipts imt ifuns <- InstanceX _ def ivs ictx inn ipts imt ifuns
    where Instance def ivs ictx inn ipts imt ifuns = InstanceX mempty def ivs ictx inn ipts imt ifuns

-- FunDef patterns
pattern FunDef :: NamedSignature -> NamedBody -> NamedFunDef
pattern FunDef sig body <- FunDefX sig body
    where FunDef sig body = FunDefX sig body
