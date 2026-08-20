module ComptimeCheckTests (comptimeCheckTests) where

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

var :: String -> MastExp
var n = MastVar (MastId (Name n) wordTy)

-- A function returning the value of its first parameter, or 1 if it has none.
fundef :: String -> [MastParam] -> Bool -> MastFunDef
fundef n ps retCt = funWithBody n ps retCt [MastReturn body]
  where
    body = case ps of
      [] -> MastLit (IntLit 1)
      (p : _) -> var (show (mastParamName p))

funWithBody :: String -> [MastParam] -> Bool -> [MastStmt] -> MastFunDef
funWithBody n ps retCt = MastFunDef (Name n) ps retCt wordTy

unitOf :: [MastFunDef] -> MastCompUnit
unitOf fds = MastCompUnit [] [MastTContr (MastContract (Name "C") (map MastCFunDecl fds))]

accepts :: String -> MastCompUnit -> TestTree
accepts name cu =
  testCase name $ checkComptime cu @?= Right ()

rejects :: String -> String -> MastCompUnit -> TestTree
rejects name expected cu =
  testCase name $ checkComptime cu @?= Left expected

comptimeCheckTests :: TestTree
comptimeCheckTests =
  testGroup
    "Comptime verification"
    [ accepts "function with no comptime annotation is accepted" $
        unitOf [fundef "f" [param False "x"] False],
      -- Partial evaluation substitutes comptime arguments and drops the
      -- parameters; one still standing means that never happened.
      rejects
        "surviving comptime parameter is rejected"
        "comptime parameter 'n' of 'f' survived partial evaluation"
        $ unitOf [fundef "f" [param True "n"] False],
      rejects
        "surviving comptime return is rejected"
        "function 'f' annotated '-> comptime' survived partial evaluation"
        $ unitOf [fundef "f" [] True],
      -- A parameter reference is not a value, so the comptime promise the
      -- return annotation makes is unmet.
      rejects
        "'-> comptime' returning a non-value is rejected"
        "function 'f' annotated '-> comptime' returns a runtime expression"
        $ unitOf [fundef "f" [param False "x"] True],
      accepts "comptime let bound to a literal is accepted" $
        unitOf
          [ funWithBody
              "f"
              []
              False
              [ MastLet True (MastId (Name "x") wordTy) (Just wordTy) (Just (MastLit (IntLit 7))),
                MastReturn (var "x")
              ]
          ],
      -- A residual call is a runtime computation whatever it computes:
      -- evaluation has already had its chance to fold it.
      rejects
        "comptime let bound to a residual call is rejected"
        "comptime let 'x' is bound to a runtime expression"
        $ unitOf
          [ fundef "g" [] False,
            funWithBody
              "f"
              []
              False
              [ MastLet True (MastId (Name "x") wordTy) (Just wordTy) (Just (MastCall (MastId (Name "g") wordTy) [])),
                MastReturn (var "x")
              ]
          ],
      rejects
        "non-value argument to a comptime parameter is rejected"
        "runtime value passed to comptime parameter 'n' of 'g'"
        $ unitOf
          [ fundef "g" [param True "n"] False,
            funWithBody
              "f"
              [param False "x"]
              False
              [MastReturn (MastCall (MastId (Name "g") wordTy) [var "x"])]
          ],
      accepts "literal argument to a comptime parameter is accepted" $
        unitOf
          [ fundef "g" [param False "n"] False,
            funWithBody
              "f"
              []
              False
              [MastReturn (MastCall (MastId (Name "g") wordTy) [MastLit (IntLit 3)])]
          ]
    ]
