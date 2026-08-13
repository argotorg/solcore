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
    fieldSetName,
    isStructDataTy,
    structFieldMap,
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

-- | Deterministic name of the field-setter function for a struct type and
-- field: @setter(s, v)@ returns @s@ with that one field replaced by @v@. Shared
-- with the field-access desugarer, which rewrites a storage field write
-- @f.x = v@ into a whole-struct write @f = setter(f, v)@ (read-modify-write).
fieldSetName :: Name -> Name -> Name
fieldSetName structTy field =
  Name ("$fieldset$" ++ seg structTy ++ "$" ++ seg field)
  where
    seg = intercalate "." . nameSegments

-- | Map from a struct type name to its field names, in declaration order, for
-- the structs declared in the given decls. Consumers (e.g. the field-access
-- desugarer) use it to recognise a struct-typed contract field and locate a
-- field by name.
structFieldMap :: [DataTy] -> [(Name, [Name])]
structFieldMap dts =
  [ (dataName dt, constrFields c)
  | dt@(DataTy _ _ [c] _) <- dts,
    isStructDataTy dt
  ]

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
  concat
    [ [ TFunDef (projectionFun dt cname tys fields i),
        TFunDef (setterFun dt cname tys fields i)
      ]
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

-- Field setter: reconstruct the struct with field i replaced by the argument.
--
--   function <set S f>(_s : S, _v : Ti) -> S {
--     match _s { | Ctor(_gv0, .., _gv_{n-1}) => return Ctor(.., _v, ..); }
--   }
setterFun :: DataTy -> Name -> [Ty] -> [Name] -> Int -> FunDef Name
setterFun dt cname tys fields i =
  FunDef False sig body
  where
    structTy = TyCon (dataName dt) (map TyVar (dataParams dt))
    fldTy = tys !! i
    fieldNm = fields !! i
    vars = [Name ("_gv" ++ show k) | k <- [0 .. length tys - 1]]
    scrutinee = Name "_s"
    newValue = Name "_v"
    -- rebuilt constructor arguments: the matched vars, with position i replaced
    rebuilt =
      [ if k == i then Var newValue else Var (vars !! k)
      | k <- [0 .. length tys - 1]
      ]
    body =
      [ Match
          [Var scrutinee]
          [([PCon cname (map PVar vars)], [Return (Con cname rebuilt)])]
      ]
    sig =
      Signature
        { sigVars = dataParams dt,
          sigContext = [],
          sigName = fieldSetName (dataName dt) fieldNm,
          sigParams =
            [ Typed False scrutinee structTy,
              Typed False newValue fldTy
            ],
          sigRetComptime = False,
          sigReturn = Just structTy,
          sigPayable = False
        }
