-- | Struct field-projection generation.
--
-- A Solidity-style @struct@ is represented internally as a single-constructor
-- product @data Foo = Foo(T1, ..., Tn)@ whose constructor also carries the
-- field names (see 'Solcore.Frontend.Syntax.Contract.Constr'). This early
-- (pre-typecheck) desugar emits one positional projection function per field:
--
-- >   function <proj Foo x>(_s : Foo) -> T1 {
-- >     match _s { | Foo(_gv0, _gv1) => return _gv0; }
-- >   }
--
-- Dot-notation access @s.x@ is rewritten to a call of the matching projection
-- during type checking (see 'Solcore.Frontend.TypeInference.TcStmt'), using the
-- same 'fieldProjName' mangling so the two sides agree. Because the projection
-- is an ordinary function whose body is a single-constructor @match@, it lowers
-- through the existing match compiler to the backend's @fst@/@snd@ projections
-- with no runtime cost.
module Solcore.Desugarer.StructProjection
  ( structProjectionTopDecls,
    fieldProjName,
    isStructDataTy,
  )
where

import Data.List (intercalate)
import Solcore.Frontend.Syntax

-- | Deterministic, collision-resistant name of the projection function for a
-- given struct type and field. Computed identically here and at each dot-access
-- site so the generated function and its callers line up.
fieldProjName :: Name -> Name -> Name
fieldProjName structTy field =
  Name ("$field$" ++ seg structTy ++ "$" ++ seg field)
  where
    seg = intercalate "." . nameSegments

-- | A struct is a single-constructor data type whose constructor carries field
-- names. Ordinary @data@ constructors have an empty 'constrFields'.
isStructDataTy :: DataTy -> Bool
isStructDataTy (DataTy _ _ [c] _) = not (null (constrFields c))
isStructDataTy _ = False

-- | Append struct field-projection functions for every struct declared in the
-- current module's own declarations. @localData@ is the whole program's local
-- data types (as gathered by the pipeline); we only emit for structs actually
-- present in @allDecls@ to avoid duplicating them across modules.
structProjectionTopDecls :: [DataTy] -> [TopDecl Name] -> [TopDecl Name]
structProjectionTopDecls localData allDecls =
  allDecls ++ concatMap projectionsForStruct structs
  where
    localNames = [dataName dt | TDataDef dt <- allDecls]
    structs =
      [ dt
      | dt <- localData,
        isStructDataTy dt,
        dataName dt `elem` localNames
      ]

projectionsForStruct :: DataTy -> [TopDecl Name]
projectionsForStruct dt@(DataTy _ _ [Constr cname tys fields] _) =
  [ TFunDef (projectionFun dt cname tys fields i)
  | i <- [0 .. length fields - 1]
  ]
projectionsForStruct _ = []

projectionFun :: DataTy -> Name -> [Ty] -> [Name] -> Int -> FunDef Name
projectionFun dt cname tys fields i =
  FunDef False sig body
  where
    structTy = TyCon (dataName dt) (map TyVar (dataParams dt))
    fldTy = tys !! i
    fieldNm = fields !! i
    vars = [Name ("_gv" ++ show k) | k <- [0 .. length tys - 1]]
    scrutinee = Name "_s"
    body =
      [ Match
          [Var scrutinee]
          [([PCon cname (map PVar vars)], [Return (Var (vars !! i))])]
      ]
    sig =
      Signature
        { sigVars = dataParams dt,
          sigContext = [],
          sigName = fieldProjName (dataName dt) fieldNm,
          sigParams = [Typed False scrutinee structTy],
          sigRetComptime = False,
          sigReturn = Just fldTy,
          sigPayable = False
        }
