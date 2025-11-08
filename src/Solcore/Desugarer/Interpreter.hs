module Solcore.Desugarer.Interpreter where
import Solcore.Desugarer.Retype
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Primitives.Primitives
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Common.Pretty

import Data.Generics


data IResult a = Value a | Thunk TcExp

interpret :: CompUnit Id -> CompUnit Id
interpret = everywhere (mkT interp)

interp :: TcExp -> TcExp
interp e@(Con n [arg])
  | idName n == Name "comptime" = runResult (eval arg)
interp e@(Call Nothing n [e1, e2])
  | idName n == Name "compplus"
    = case (interp e1, interp e2) of
        (Lit (IntLit n1), Lit (IntLit n2)) -> Con (Id (Name "comptime") (word :-> (compTimeTy word))) [Lit (IntLit (n1 + n2))]
        _ -> e
interp e = e

runResult :: IResult Integer -> TcExp
runResult (Value n) = Lit (IntLit n)
runResult (Thunk e) = e

eval :: TcExp -> IResult Integer
eval e@(Lit (IntLit n)) = Value n
eval e@(Call Nothing n es)
  | idName n == Name "primAddWord"
    = case map eval es of
        [Value v1, Value v2] ->  Value (v1 + v2)
        _ -> Thunk e
  | otherwise = Thunk e

