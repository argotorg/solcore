module ComptimeCheckTests (comptimeCheckTests) where

import Data.Either (isLeft, isRight)
import Solcore.Backend.ComptimeCheck (checkComptime)
import Solcore.Backend.Mast
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt (Literal (..))
import Test.Tasty
import Test.Tasty.HUnit

wordTy :: MastTy
wordTy = MastTyCon (Name "word") []

param :: Bool -> String -> MastParam
param ct n = MastParam (Name n) ct wordTy

-- A function returning the value of its first parameter, or 1 if it has none.
fundef :: String -> [MastParam] -> Bool -> MastFunDef
fundef n ps retCt = MastFunDef (Name n) ps retCt wordTy [MastReturn body]
  where
    body = case ps of
      [] -> MastLit (IntLit 1)
      (p : _) -> MastVar (MastId (mastParamName p) wordTy)

unitOf :: [MastFunDef] -> MastCompUnit
unitOf fds = MastCompUnit [] [MastTContr (MastContract (Name "C") (map MastCFunDecl fds))]

comptimeCheckTests :: TestTree
comptimeCheckTests =
  testGroup
    "Comptime verification"
    [ testCase "function with no comptime annotation is accepted" $
        assertBool "expected acceptance" $
          isRight (checkComptime (unitOf [fundef "f" [param False "x"] False])),
      -- Partial evaluation substitutes comptime arguments and drops the
      -- parameters; one still standing means that never happened.
      testCase "surviving comptime parameter is rejected" $
        assertBool "expected rejection" $
          isLeft (checkComptime (unitOf [fundef "f" [param True "n"] False])),
      testCase "surviving comptime return is rejected" $
        assertBool "expected rejection" $
          isLeft (checkComptime (unitOf [fundef "f" [] True]))
    ]
