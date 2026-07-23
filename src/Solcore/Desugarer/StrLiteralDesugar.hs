module Solcore.Desugarer.StrLiteralDesugar (desugarStrLiterals) where

import Data.Generics
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.Traversal (everywhereButSpans)
import Solcore.Primitives.Primitives (strClassName)

-- Wrap comptime string values in expression position with a call to
-- Str.fromString, making them polymorphic over their target representation:
--   * every string literal, and
--   * every comptime concatenation (`concatLit(...)`).
-- At a `string` site the wrapper resolves to the identity instance (and folds);
-- at a `memory(string)` site it materializes.  Because `concatLit`'s parameters
-- are `string`, a wrapped `concatLit` used as an argument to another `concatLit`
-- resolves to identity, so nested concatenations collapse to a single literal
-- and materialize once.  Pattern literals (PLit) are not expressions and are
-- left untouched.
desugarStrLiterals :: [TopDecl Name] -> [TopDecl Name]
desugarStrLiterals = everywhereButSpans (mkT desugarExp)

fromStringName :: Name
fromStringName = QualName strClassName "fromString"

-- | Recognise concatLit regardless of qualification (e.g. @std.concatLit@).
isConcatLit :: Name -> Bool
isConcatLit (Name "concatLit") = True
isConcatLit (QualName _ "concatLit") = True
isConcatLit _ = False

desugarExp :: Exp Name -> Exp Name
desugarExp (Lit l@(StrLit _)) = Call Nothing fromStringName [Lit l]
desugarExp e@(Call _ n _) | isConcatLit n = Call Nothing fromStringName [e]
desugarExp e = e
