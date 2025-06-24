module Solcore.Frontend.Pretty.Name where
import Common.Pretty
import Solcore.Frontend.Syntax.Name


instance Pretty Name where
  ppr (QualName n s) = ppr n <> text "." <> text s
  ppr (Name s) = text s


