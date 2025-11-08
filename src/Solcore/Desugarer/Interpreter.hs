module Solcore.Desugarer.Interpreter where
import Solcore.Desugarer.Retype
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Primitives.Primitives
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Common.Pretty

data IResult a = Value a | Thunk TcExp 

eval :: TcExp -> IResult TcExp
eval e = undefined
