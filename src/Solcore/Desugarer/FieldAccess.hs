module Solcore.Desugarer.FieldAccess where

import Control.Monad.Identity

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Primitives.Primitives

fieldDesugarer :: CompUnit Name -> CompUnit Name
fieldDesugarer cu = cu
