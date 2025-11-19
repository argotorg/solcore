{-# LANGUAGE OverloadedRecordDot #-}
module Solcore.Desugarer.FieldAccess(fieldDesugarer) where

import Control.Monad.Reader()
import Data.List (mapAccumL, foldl')
import Data.Map qualified as Map
import Data.Map(Map)
-- import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Set (Set)

import GHC.Stack

import Text.Pretty.Simple

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Primitives.Primitives

type ContractName = Name
type NmContract = Contract Name
type NmField = Field Name
type NmTopDecl = TopDecl Name
type NmContractDecl = ContractDecl Name
type NmBody = Body Name
type NmStmt = Stmt Name
type NmExp = Exp Name

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
               , paramsTy = [fty, offset]
               , mainTy = TyCon "StructField" [ctxTy, selType]
               , instFunctions = []
               }

--------------------------------
-- # Contract Desugaring
--------------------------------

data ContractEnv = CEnv { ceName :: Name, ceFields :: Map Name NmField, ceLocals :: Set Name }

askFieldTy :: Name -> ContractEnv -> Maybe Ty
askFieldTy x env = fieldTy <$> Map.lookup x env.ceFields

transContract :: NmContract -> NmContract
transContract c = c { decls = concatMap (flip transCDecl cenv) c.decls } where
    cenv = CEnv { ceName= c.name
                , ceFields = Map.fromList [ (fieldName f, f) | f <- getFields c.decls]
                , ceLocals = mempty
                }

transCDecl :: NmContractDecl -> ContractEnv -> [NmContractDecl]
transCDecl (CFunDecl fd) = do
  body' <- transBody fd.funDefBody
  pure [CFunDecl fd { funDefBody = body' }]
transCDecl CFieldDecl{} = pure []
transCDecl d = pure [d]

transBody :: NmBody -> ContractEnv -> NmBody
transBody body cenv = snd $  mapAccumL transStmt cenv body

transStmt :: ContractEnv -> NmStmt -> (ContractEnv, NmStmt)
transStmt cenv stmt@(Let x _ _) = (cenv{ceLocals = Set.insert x cenv.ceLocals}, stmt)
transStmt cenv (lhs := rhs) = (cenv, transAssignment lhs rhs cenv)
transStmt cenv stmt = (cenv, stmt)

transAssignment :: NmExp -> NmExp -> ContractEnv -> NmStmt
transAssignment lhs rhs = pure(lhs := rhs)

--------------------------------
-- # Helpers
--------------------------------

getFields :: [NmContractDecl] -> [NmField]
getFields cdecls = concatMap getF cdecls where
    getF (CFieldDecl f) = [f]
    getF _ = []

appendToName :: Name -> String -> Name
appendToName (Name s) t = Name (s <> t)
appendToName (QualName n s) t = QualName n (s <> t)

selectorNameForField :: Name -> Name -> Name
selectorNameForField cname (Name fld) = Name(show cname <> fld <> "_sel")
selectorNameForField _ n = notImplementedS "selectorNameForField" n

singletonNameForContract :: Name -> Name
singletonNameForContract cname = appendToName cname "Cxt"

singletonTypeForContract :: Name -> Ty
singletonTypeForContract cname = TyCon (singletonNameForContract cname) []

singletonValForContract :: Name -> Exp Name
singletonValForContract cname = Con (singletonNameForContract cname) []

-- notImplemented :: (HasCallStack, Pretty a) => String -> a -> b
-- notImplemented funName a = error $ concat [funName, " not implemented yet for ", pretty a]

notImplementedS :: (HasCallStack, Show a) => String -> a -> b
notImplementedS funName a = error $ concat [funName, " not implemented yet for ", show(pShow a)]
