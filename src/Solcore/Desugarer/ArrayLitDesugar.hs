-- Expansion of array literals, after type checking.
--
-- The type checker gives [e1, ..., en] the type memory(DynArray(t)) and
-- wraps it in a TyExp carrying that type.  This pass turns each literal into
-- a chain of calls to the std-library builders:
--
-- arrayLitInit(... arrayLitInit(arrayLitNew(n), 0, e1) ..., n-1, en)
--
-- Each step returns the array it wrote to, so the chain is a plain expression
-- and needs no statement sequencing.  Allocation happens first and the elements
-- are then evaluated left to right; a nested literal in an element is safe
-- because memory is bump allocated.
--
-- Running after type checking means the array type is already resolved, so the
-- emitted calls can be given concrete types directly.
module Solcore.Desugarer.ArrayLitDesugar (arrayLitDesugarer) where

import Data.Generics
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.Traversal (everywhereButSpans)
import Solcore.Frontend.TypeInference.Id
import Solcore.Primitives.Primitives (intClassName, integer)

arrayLitDesugarer :: CompUnit Id -> CompUnit Id
arrayLitDesugarer = everywhereButSpans (mkT desugarExp)

desugarExp :: Exp Id -> Exp Id
desugarExp (TyExp (ArrayLit es) arrTy)
  | Just elemTy <- dynArrayElemTy arrTy = buildArray arrTy elemTy es
desugarExp e = e

dynArrayElemTy :: Ty -> Maybe Ty
dynArrayElemTy (TyCon (Name "memory") [TyCon (Name "DynArray") [t]]) = Just t
dynArrayElemTy _ = Nothing

buildArray :: Ty -> Ty -> [Exp Id] -> Exp Id
buildArray arrTy elemTy es =
  foldl initElem emptyArray (zip [0 ..] es)
  where
    emptyArray =
      Call
        Nothing
        (Id arrayLitNewName (uint256 :-> arrTy))
        [uintLit (length es)]
    initElem acc (i, e) =
      Call
        Nothing
        (Id arrayLitInitName (arrTy :-> uint256 :-> elemTy :-> arrTy))
        [acc, uintLit i, e]

-- Integer literals reaching this point are still integer; the concrete type
-- comes from Int.fromInteger, exactly as IntLiteralDesugar arranges for source
-- literals.  MastEval folds these away at compile time.
uintLit :: Int -> Exp Id
uintLit n =
  Call
    Nothing
    (Id (QualName intClassName "fromInteger") (integer :-> uint256))
    [Lit (IntLit (toInteger n))]

uint256 :: Ty
uint256 = TyCon (Name "uint256") []

arrayLitNewName :: Name
arrayLitNewName = Name "arrayLitNew"

arrayLitInitName :: Name
arrayLitInitName = Name "arrayLitInit"
