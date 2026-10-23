{-# LANGUAGE OverloadedStrings #-}

module ClosureCaptureTests (closureCaptureTests) where

import Language.Hull.TcEnv (emptyHullTcEnv)
import Language.Hull.TcMonad (runHullTcM)
import Language.Hull.TypeCheck (checkObject)
import Language.Yul
import Solcore.Pipeline.Options
import Solcore.Pipeline.SolcorePipeline (compile)
import Test.Tasty
import Test.Tasty.HUnit

closureCaptureTests :: TestTree
closureCaptureTests =
  testGroup
    "Assembly closure captures"
    [ testCase "reads and assignment targets escape; call names do not" $
        freeYulVariables [YAssign ["out"] (YCall "add" [YIdent "x", YLit (YulNumber 1)])]
          @?= ["out", "x"],
      testCase "initializer sees outer name before local declaration" $
        freeYulVariables [YLet ["x"] (Just (YIdent "x")), YExp (YIdent "x")]
          @?= ["x"],
      testCase "nested locals do not hide later outer references" $
        freeYulVariables [YBlock [YLet ["x"] Nothing, YAssign ["x"] (YLit (YulNumber 1))], YExp (YIdent "x")]
          @?= ["x"],
      testCase "function arguments and returns remain local" $
        freeYulVariables [YFun "helper" ["x"] (Just ["out"]) [YAssign ["out"] (YIdent "x")], YExp (YCall "helper" [YIdent "input"])]
          @?= ["input"],
      testCase "loop initializer bindings cover condition, post and body only" $
        freeYulVariables [YFor [YLet ["i"] (Just (YIdent "start"))] (YIdent "i") [YAssign ["i"] (YIdent "step")] [YExp (YIdent "i")], YExp (YIdent "i")]
          @?= ["start", "step", "i"],
      testCase "switch arms have independent locals" $
        freeYulVariables [YSwitch (YIdent "tag") [(YulNumber 0, [YLet ["x"] Nothing])] (Just [YExp (YIdent "x")])]
          @?= ["tag", "x"],
      checkedHull "inc-closure.sol",
      checkedHull "asm-closure-captures.sol"
    ]
  where
    checkedHull file = testCase (file ++ " has well-bound Hull") $ do
      let folder = "test/examples/cases"
          opts = stdOpt {fileName = folder ++ "/" ++ file, optRootDir = folder, optNoGenDispatch = True}
      result <- compile opts
      case result of
        Left err -> assertFailure err
        Right objects ->
          mapM_
            ( \obj -> do
                checked <- runHullTcM (checkObject obj) emptyHullTcEnv
                either assertFailure pure checked
            )
            objects
