module Solcore.Desugarer.IntLiteralDesugar (desugarIntLiterals) where

import Data.Generics
import Solcore.Frontend.Syntax
import Solcore.Primitives.Primitives (intClassName)

-- Wrap every integer literal in expression position with a call to
-- Int.fromInteger, making integer literals polymorphic.  Pattern
-- literals (PLit) are not expressions and are left untouched; they are
-- handled separately by the type checker.
desugarIntLiterals :: [TopDecl Name] -> [TopDecl Name]
desugarIntLiterals = everywhere (mkT desugarExp)

fromIntegerName :: Name
fromIntegerName = QualName intClassName "fromInteger"

desugarExp :: Exp Name -> Exp Name
desugarExp (Lit l@(IntLit _)) = Call Nothing fromIntegerName [Lit l]
desugarExp e = e
