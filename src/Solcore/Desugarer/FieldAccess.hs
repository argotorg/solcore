{-# LANGUAGE OverloadedRecordDot #-}
module Solcore.Desugarer.FieldAccess(fieldDesugarer) where
import Prelude hiding(exp)

import Control.Monad.Reader(MonadReader(..))
-- import Data.Generics(Data, mkT, everywhere)
import Data.List (mapAccumL, foldl')
import Data.Map qualified as Map
import Data.Map(Map)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Set (Set)

import Debug.Trace
import GHC.Stack

import Text.Pretty.Simple

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding(name)
import qualified Solcore.Frontend.Syntax.Contract as Contract
import Solcore.Primitives.Primitives hiding(arr)

type ContractName = Name
type NmContract = Contract Name
type NmField = Field Name
type NmTopDecl = TopDecl Name
type NmContractDecl = ContractDecl Name
type NmBody = Body Name
type NmStmt = Stmt Name
type NmExp = Exp Name
type NmEquation = Equation Name

fieldDesugarer :: CompUnit Name -> CompUnit Name
fieldDesugarer (CompUnit ims topdecls) = CompUnit ims (extras <> topdecls')
  where
    (extras, topdecls') = mapAccumL go mempty topdecls
    go acc (TContr c) = (acc <> extraTopDeclsForContract c, TContr (transContract c))
    go acc v = (acc, v)

--------------------------------
-- # Extra Top Decls
--------------------------------

extraTopDeclsForContract :: NmContract -> [NmTopDecl]
extraTopDeclsForContract (Contract cname _ts cdecls) = do
    let singName = singletonNameForContract cname
    let contractSingDecl = TDataDef $ DataTy singName [] [Constr singName []]

    let fields = getFields cdecls
    let (_fieldTypes, extraFieldDecls) = foldl' (flip contractFieldStep) ([], []) fields
    (contractSingDecl:extraFieldDecls)
    where
      -- given a list of contract field types so far and topdecls for them, amends them with data for another field
      -- the types of previous fields are needed to construct field offset
      contractFieldStep :: NmField -> ([Ty], [NmTopDecl]) -> ([Ty], [NmTopDecl])
      contractFieldStep field (tys, topdecls) = (tys', topdecls') where
          tys' = tys ++ [fieldTy field]
          topdecls' = topdecls ++ extraTopDeclsForContractField cname field offset
          offset = foldr pair unit tys


extraTopDeclsForContractField :: ContractName -> NmField -> Ty -> [NmTopDecl]
extraTopDeclsForContractField cname (Field fname fty _minit) offset = [selDecl, TInstDef sfInstance] where
  -- data b_sel = n_sel
  selName = selectorNameForField cname fname
  selDecl = TDataDef $ DataTy selName [] [Constr selName []]
  selType = TyCon selName []
  -- instance StructField(ContractStorage(CCtx), fld1_sel):StructField(uint, ()) {}
  ctxTy = TyCon "ContractStorage" [singletonTypeForContract cname]
  sfInstance = Instance
               { instDefault = False
               , instVars = []
               , instContext = []
               , instName = "StructField"
               , paramsTy = [translateFieldType fty, offset]
               , mainTy = TyCon "StructField" [ctxTy, selType]
               , instFunctions = []
               }

translateFieldType :: Ty -> Ty
translateFieldType t = TyCon "storage" [t]

--------------------------------
-- # Contract Desugaring
--------------------------------
-- the desugaring behaves mostly like a Reader, but semetimes we want to make the environment explicit
-- Note that we cannot simply use `everywhere` - this would require at least three passes
-- - desugar assignments
-- - desugar indexing
-- - desugar field accesses
-- lest we inadvertenly mistranslate a LHS as a RHS

data ContractEnv = CEnv { ceName :: Name, ceFields :: Map Name NmField, ceLocals :: Set Name }
type CEM a = ContractEnv -> a

transContract :: NmContract -> NmContract
transContract c = c { decls = concatMap (flip transCDecl cenv) (Contract.decls c) } where
    cenv = CEnv { ceName = Contract.name c
                , ceFields = Map.fromList [ (fieldName f, f) | f <- getFields (Contract.decls c)]
                , ceLocals = mempty
                }

transCDecl :: NmContractDecl -> CEM [NmContractDecl]
transCDecl (CFunDecl fd) = do
  body' <- transBody fd.funDefBody
  pure [CFunDecl fd { funDefBody = body' }]
transCDecl (CConstrDecl cd) = do
  body' <- transBody cd.constrBody
  pure [CConstrDecl cd { constrBody = body' }]
transCDecl CFieldDecl{} = pure []
transCDecl d = pure [d]

transBody :: NmBody -> ContractEnv -> NmBody
transBody body cenv = snd $  mapAccumL transStmt cenv body

transStmt :: ContractEnv -> NmStmt -> (ContractEnv, NmStmt)
transStmt cenv (Let x mty me) = (cenv{ceLocals = Set.insert x cenv.ceLocals}, Let x mty me') where
    me' = flip transRhs cenv <$> me

transStmt cenv stmt = (cenv, go stmt cenv) where
    go :: NmStmt -> CEM NmStmt
    go (lhs := rhs) = transAssignment lhs rhs
    go (Return exp) = Return <$> transRhs exp
    go (StmtExp exp) = StmtExp <$> transRhs exp
    go (If e b1 b2) = If <$> transRhs e <*> transBody b1 <*> transBody b2
    go (Match es eqns) = traces [pretty (r cenv)] r where r =Match <$> mapM transRhs es <*> mapM transEquation eqns
    go Let{} = error "Impossible"
    go s@Asm{} = pure s
    -- go s = pure s

transEquation :: NmEquation -> CEM NmEquation
transEquation (pats, body) cenv = (pats, transBody body cenv)

transAssignment :: NmExp -> NmExp -> ContractEnv -> NmStmt
transAssignment lhs@(Var x) rhs cenv
  | isLocal x cenv =
      traces ["tA: Assignment to local var ", show x]
      (lhs := rhs')
  | Just _fty <- askFieldTy x cenv =
      traces ["tA: Assignment to variable which is a field and not local:", show x]
      (lhs := rhs')
  where rhs' = transRhs rhs cenv
transAssignment lhs@(FieldAccess Nothing x) rhs cenv
  | isLocal x cenv =
      traces ["tA: Assignment to a field which is shadowed by local var:", pretty x]
      (Var x := rhs')
  | Just _fty <- askFieldTy x cenv =
      traces ["tA: Assignment a contract field which is not local:",  pretty (lhs := rhs)]
      (transContractFieldAssignment x rhs' cenv)
  where rhs' = transRhs rhs cenv

transAssignment (Indexed arr idx) rhs cenv = do
  let idx' = traces ["transRhs", pretty idx] $ transRhs idx cenv
  let lhs' = traces ["lhsIndex", pretty arr, pretty idx] $ lhsIndex arr idx' cenv
  let rhs' = traces ["transRhs", pretty rhs] $ transRhs rhs cenv
  let assignName = QualName (Name "Assign") "assign"
  StmtExp $ Call Nothing assignName [lhs', rhs']

transAssignment lhs rhs cenv =
    traces ["Other assignment:", pretty (lhs := rhs)] $
    (lhs := rhs')
    where rhs' = transRhs rhs cenv

transContractFieldAssignment :: Name -> NmExp -> CEM NmStmt
transContractFieldAssignment field rhs = do
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
  rhs' <- transRhs rhs
  let assignName = QualName (Name "Assign") "assign"
  pure $ StmtExp $ Call Nothing assignName [lhs', rhs']

transRhs :: HasCallStack => NmExp -> CEM NmExp
transRhs expr@(FieldAccess Nothing x) cenv
  | isLocal x cenv = traces ["Local:", pretty x] (Var x)
  | Just _fty <- askFieldTy x cenv = let
      cname = ceName cenv
      cxt = contractContext cenv
      -- rvalFun = Name "rval"
      fieldSel = Con (selectorNameForField cname x) []
      fieldMap = Con "MemberAccessProxy" [cxt, fieldSel]
      result = rhsAccess fieldMap
      in traces ["< transRhs", pretty expr, "~>", pretty result] result

transRhs expr@FieldAccess{} _ = notImplemented "transRhs" expr
transRhs expr cenv = go expr cenv where
    go e@(Indexed arr idx) = \env -> let e' = rhsIndex arr idx env in traces ["transRhs", pretty e,  "- rhsIndex ->", pretty e'] e' -- FIXME
    go (Call me f as) = Call me f <$> mapM transRhs as
    go (Lam ps b mty) = Lam ps <$> transBody b <*> pure mty
    go (TyExp e ty) = TyExp <$> transRhs e <*> pure ty
    go (Cond e1 e2 e3) = Cond <$> transRhs e1 <*> transRhs e2 <*> transRhs e3
    go e@Var{} = pure e
    go e@Con{} = pure e
    go e@Lit{} = pure e
    go FieldAccess{} = error "Impossible"

indexFun :: Either () () -> Name
indexFun Left{}  = (Name "lidx")
indexFun Right{} = (Name "ridx")

indexAccess :: HasCallStack => Either () () -> NmExp -> NmExp -> CEM (Exp Name)
indexAccess dir exp@(FieldAccess Nothing name) idx = traces ["iA FA: " ++ pretty name ++ " " ++ pretty idx] $ do
  isF <- isField name
  if isF then do
    arrProxy <- memberProxyFor name
    let arrRef = lhsAccess arrProxy
    idx' <- transRhs idx
    pure $ Call Nothing (indexFun dir) [arrRef, idx']
  else notImplemented "indexAccess" exp

indexAccess dir _exp@(Indexed arr1 idx1) idx2 = traces ["iA II:", pretty arr1,pretty idx1, pretty idx2] $ do
   idx2' <- traces ["transRhs", pretty idx2 ] $ transRhs idx2
   idx1' <- traces ["transRhs", pretty idx1 ] $ transRhs idx1
   arr' <- traces ["lhsIndex", pretty arr1, pretty idx1'] $ lhsIndex arr1 idx1'
   pure $ Call Nothing (indexFun dir) [arr', idx2']

indexAccess _dir exp idx = notImplemented "indexAccess" (Indexed exp idx)

lhsIndex, rhsIndex :: HasCallStack => NmExp -> NmExp -> CEM (Exp Name)
lhsIndex = indexAccess $ Left ()
rhsIndex = indexAccess $ Right ()


--------------------------------
-- # Helpers
--------------------------------

askFieldTy :: Name -> ContractEnv -> Maybe Ty
askFieldTy x env = fieldTy <$> Map.lookup x env.ceFields

isField :: Name -> ContractEnv -> Bool
isField x env = isJust $ askFieldTy x env

isLocal :: Name -> ContractEnv -> Bool
isLocal x env = Set.member x env.ceLocals

getFields :: [NmContractDecl] -> [NmField]
getFields cdecls = concatMap getF cdecls where
    getF (CFieldDecl f) = [f]
    getF _ = []

appendToName :: Name -> String -> Name
appendToName (Name s) t = Name (s <> t)
appendToName (QualName n s) t = QualName n (s <> t)

selectorNameForField :: Name -> Name -> Name
selectorNameForField cname (Name fld) = Name(show cname <> "_" <> fld <> "_sel")
selectorNameForField _ n = notImplementedS "selectorNameForField" n

singletonNameForContract :: Name -> Name
singletonNameForContract cname = appendToName cname "Cxt"

singletonTypeForContract :: Name -> Ty
singletonTypeForContract cname = TyCon (singletonNameForContract cname) []

-- singletonValForContract :: Name -> NmExp
-- singletonValForContract cname = Con (singletonNameForContract cname) []

contractContext :: CEM NmExp
contractContext = do
  cname <- reader ceName
  let singName = singletonNameForContract cname
  -- let contractSingTy = TyCon singName []
  let contractSing = Con singName []
  -- let cxtTy = TyCon "ContractStorage" [contractSingTy]
  let cxt = Con "ContractStorage" [contractSing]
  pure cxt

memberProxyFor :: Name -> CEM NmExp
memberProxyFor field = do
  cname <- reader ceName
  cxt <- contractContext
  let selName = selectorNameForField cname field
  let selector = Con selName []
  let fieldMap = Con "MemberAccessProxy" [cxt, selector]
  pure fieldMap

lhsAccess :: Exp Name -> Exp Name
lhsAccess e = Call Nothing (QualName "LVA" "acc") [e]

rhsAccess :: Exp Name -> Exp Name
rhsAccess e = Call Nothing (QualName "RVA" "acc") [e]

notImplemented :: (HasCallStack, Pretty a) => String -> a -> b
notImplemented funName a = error $ concat [funName, " not implemented yet for ", pretty a]

notImplementedS :: (HasCallStack, Show a) => String -> a -> b
notImplementedS funName a = error $ concat [funName, " not implemented yet for ", show(pShow a)]

traces :: [String] -> a -> a
-- traces = trace . unwords
traces _ a = a
