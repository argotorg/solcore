module Solcore.Frontend.Pretty.ShortName where
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Pretty.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.Pretty.SolcorePretty(pretty)

import Common.Pretty
prettys :: Pretty a => [a] -> String
prettys = render . brackets . commaSep . map ppr

class Pretty a => HasShortName a where
  shortName :: a -> String
  shortName = pretty

instance HasShortName Name
instance HasShortName Id

instance HasShortName a => HasShortName (Contract a) where
    shortName (Contract n _ _) = shortName n

instance HasShortName a => HasShortName (Signature a) where
    shortName sig = shortName (sigName sig)

instance HasShortName a => HasShortName (FunDef a) where
    shortName fd = "function " ++shortName (funSignature fd)

instance HasShortName a => HasShortName (Instance a) where
  shortName is = unwords
    [ "instance"
    , pretty (mainTy is)
    , ":"
    , pretty (instName is)
    , prettys (paramsTy is)
    ]
instance HasShortName a => HasShortName (TopDecl a) where
  shortName (TContr c) = shortName c
  shortName (TFunDef fd) = shortName fd
  shortName (TClassDef c) = shortName (className c)
  shortName (TInstDef is) = shortName is
  shortName (TMutualDef ts) = concatMap shortName ts
  shortName (TDataDef d) = pretty (dataName d)
  shortName (TPragmaDecl p) = pretty p
