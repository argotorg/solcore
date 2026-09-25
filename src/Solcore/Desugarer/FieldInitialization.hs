{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Reject contract fields of a non-primitive (algebraic) data type that are
-- never given an initial value.
--
-- A contract field with no initializer defaults to the all-zero storage slot.
-- For a word-like primitive (@uint256@, @int*@, @address@, @bytes*@, a
-- @mapping@ or @array@ handle, ... every one represented as a newtype over a
-- single machine @word@) that zero is the conventional default and is left
-- implicit, exactly as in Solidity.
--
-- For a genuine algebraic data type it is not: the zero slot decodes as the
-- /first-declared/ constructor with a zero payload, a value the field
-- declaration never states and that silently changes if the type's
-- constructors are reordered.  This module reports 'SC0233' for any such field
-- that is neither given a field initializer (@f : T = e;@) nor assigned in the
-- contract constructor, forcing the author to state the intended initial value.
module Solcore.Desugarer.FieldInitialization
  ( checkFieldInitialization,
  )
where

import Data.Generics (listify)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Solcore.Diagnostics
import Solcore.Frontend.Pretty.SolcorePretty (pretty)
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.Contract qualified as Contract

checkFieldInitialization ::
  [TopDecl Name] ->
  [TopDecl Name] ->
  Either CompilerError ()
checkFieldInitialization registrySource localDecls =
  case concatMap contractDiagnostics [c | TContr c <- localDecls] of
    [] -> Right ()
    diags -> Left (diagnosticsCompilerError diags)
  where
    registry :: Map Name DataTy
    registry = Map.fromList [(dataName dt, dt) | dt <- allDataTys registrySource]

    contractDiagnostics :: Contract Name -> [Diagnostic]
    contractDiagnostics c =
      case Contract.contractKind c of
        ContractKind ->
          [ uninitializedFieldDiagnostic f
          | f <- contractFields cdecls,
            fieldInit f == Nothing,
            fieldName f `Set.notMember` assigned,
            fieldNeedsInit registry (fieldTy f)
          ]
        _ -> []
      where
        cdecls = Contract.decls c
        assigned = constructorAssignedNames cdecls

allDataTys :: [TopDecl Name] -> [DataTy]
allDataTys = concatMap go
  where
    go (TDataDef dt) = [dt]
    go (TContr c) = [dt | CDataDecl dt <- Contract.decls c]
    go (TMutualDef ds) = concatMap go ds
    go _ = []

contractFields :: [ContractDecl Name] -> [Field Name]
contractFields cdecls = [f | CFieldDecl f <- cdecls]

constructorAssignedNames :: [ContractDecl Name] -> Set Name
constructorAssignedNames cdecls =
  Set.fromList
    [ n
    | CConstrDecl cd <- cdecls,
      (lhs := _) <- listify isAssign cd.constrBody,
      n <- listify (const True :: Name -> Bool) lhs
    ]
  where
    isAssign :: Stmt Name -> Bool
    isAssign (_ := _) = True
    isAssign _ = False

fieldNeedsInit :: Map Name DataTy -> Ty -> Bool
fieldNeedsInit registry ty = not (tyIsPrimitiveLike registry Set.empty ty)

tyIsPrimitiveLike :: Map Name DataTy -> Set Name -> Ty -> Bool
tyIsPrimitiveLike registry seen ty =
  case tyConNameOf ty of
    Nothing -> True
    Just n
      | n `Set.member` seen -> False
      | otherwise ->
          case Map.lookup n registry of
            Nothing -> True
            Just (DataTyWithKind _ _ _ constrs) ->
              case constrs of
                [] -> True
                [Constr _ [fieldT]] -> tyIsPrimitiveLike registry (Set.insert n seen) fieldT
                _ -> False

tyConNameOf :: Ty -> Maybe Name
tyConNameOf (TyCon n _) = Just n
tyConNameOf _ = Nothing

uninitializedFieldDiagnostic :: Field Name -> Diagnostic
uninitializedFieldDiagnostic f =
  Diagnostic
    { diagnosticSeverity = Error,
      diagnosticCode = Just (DiagnosticCode "SC0233"),
      diagnosticMessage =
        "contract field '"
          ++ pretty (fieldName f)
          ++ "' of algebraic data type '"
          ++ pretty (fieldTy f)
          ++ "' is never initialized",
      diagnosticLabels = fieldLabels f,
      diagnosticNotes =
        [ "an uninitialized field reads back as the all-zero storage slot; for a "
            ++ "sum type that is its first-declared constructor -- a value the "
            ++ "declaration does not state and that changes if the type is reordered"
        ],
      diagnosticHelp =
        [ "give it a field initializer, e.g. '"
            ++ pretty (fieldName f)
            ++ " : "
            ++ pretty (fieldTy f)
            ++ " = <expr>;'",
          "or assign it in the contract constructor"
        ]
    }

fieldLabels :: Field Name -> [Label]
fieldLabels f =
  case sourceSpanOf f of
    Nothing -> []
    Just sp ->
      [ Label
          { labelSpan = sp,
            labelStyle = Primary,
            labelMessage = Just "this field needs an explicit initial value"
          }
      ]
