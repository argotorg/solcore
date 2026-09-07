-- | Generate functional setters for named product fields. Typed field readers
-- are registered by TcMonad; storage updates use these setters to reconstruct
-- a product value before storing it back through the existing storage API.
module Solcore.Desugarer.StructProjection
  ( structSetterTopDecls,
    fieldSetName,
    structFieldMap,
  )
where

import Data.List (intercalate)
import Solcore.Frontend.Syntax

fieldSetName :: Name -> Name -> Name
fieldSetName structTy field =
  QualName structTy ("$structSet$" ++ intercalate "$" (nameSegments field))

structFieldMap :: [DataTy] -> [(Name, [Name])]
structFieldMap dts =
  [ (dataName dt, fields)
  | dt <- dts,
    Just (_, _, fields) <- [namedProduct dt]
  ]

namedProduct :: DataTy -> Maybe (Name, [Ty], [Name])
namedProduct (DataTyWithKind (StructKind fields) _ _ [Constr constructor types]) =
  Just (constructor, types, fields)
namedProduct (DataTy _ _ [ConstrWithFields constructor types fields])
  | not (null fields) = Just (constructor, types, fields)
namedProduct _ = Nothing

-- Each module emits only the setters for products it declares. Imported
-- products keep the setters generated in their defining module.
structSetterTopDecls :: [DataTy] -> [TopDecl Name] -> [TopDecl Name]
structSetterTopDecls localData allDecls =
  allDecls ++ concatMap settersForProduct ownProducts
  where
    localNames = concatMap declaredNames allDecls
    declaredNames (TDataDef dt) = [dataName dt]
    declaredNames (TContr (Contract _ _ declarations)) = [dataName dt | CDataDecl dt <- declarations]
    declaredNames (TMutualDef declarations) = concatMap declaredNames declarations
    declaredNames _ = []
    ownProducts = [dt | dt <- localData, dataName dt `elem` localNames]
    settersForProduct dt =
      case namedProduct dt of
        Just (constructor, types, fields) ->
          [TFunDef (setterFun dt constructor types fields index) | index <- [0 .. length fields - 1]]
        Nothing -> []

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
        (dataParams dt)
        []
        (fieldSetName (dataName dt) fieldNm)
        [ Typed False scrutinee structTy,
          Typed False newValue fldTy
        ]
        False
        (Just structTy)
        False
