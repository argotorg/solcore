module YulParserTests (yulParserTests) where

import Common.LightYear (Parser, runParserE)
import Language.Yul
import Language.Yul.Parser (yulExp, yulStmt)
import Solcore.Frontend.Syntax.Name (pattern Name)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (eof)

-- The Yul parsers (yulExp / yulStmt) consume their own leading and trailing
-- whitespace, so we only need to pin the end with eof.
parsesAs :: (Show a, Eq a) => Parser a -> String -> a -> Assertion
parsesAs p src expected =
  case runParserE (p <* eof) "<test>" src of
    Left err -> assertFailure ("Parse error:\n" ++ err)
    Right got -> assertEqual ("parsing: " ++ show src) expected got

parseFails :: (Show a) => Parser a -> String -> Assertion
parseFails p src =
  case runParserE (p <* eof) "<test>" src of
    Left _ -> return ()
    Right got -> assertFailure ("Expected failure but parsed: " ++ show got)

yIdent :: String -> YulExp
yIdent = YIdent . Name

yulParserTests :: TestTree
yulParserTests =
  testGroup
    "Yul parser"
    [ functionKeywordTests,
      boolLiteralTests,
      switchTests,
      statementKeywordTests
    ]

-- Bug 1: yulFun matched the `function` keyword with `symbol "function"`, which
-- has no word-boundary check. A statement beginning with an identifier that
-- merely starts with `function` (e.g. `functional(x)`) must NOT be treated as a
-- function definition; it should parse as an ordinary call expression statement.
functionKeywordTests :: TestTree
functionKeywordTests =
  testGroup
    "function keyword boundary"
    [ testCase "identifier with function prefix parses as a call statement" $
        parsesAs yulStmt "functional(x)" (YExp (YCall (Name "functional") [yIdent "x"])),
      -- Regression guard: a real function definition still parses.
      testCase "genuine function definition still parses" $
        parsesAs
          yulStmt
          "function f(x) { }"
          (YFun (Name "f") [Name "x"] Nothing [])
    ]

-- Bug 2: `pKeyword "true"/"false"` ran `string` (consuming input) before the
-- `notFollowedBy identChar` boundary check, so an identifier like `trueFlag`
-- failed instead of falling through to YIdent.
boolLiteralTests :: TestTree
boolLiteralTests =
  testGroup
    "true/false keyword boundary"
    [ testCase "identifier with true prefix parses as identifier" $
        parsesAs yulExp "trueFlag" (yIdent "trueFlag"),
      testCase "identifier with false prefix parses as identifier" $
        parsesAs yulExp "falseFlag" (yIdent "falseFlag"),
      -- Regression guards: the bare literals still parse as literals.
      testCase "bare true parses as literal" $
        parsesAs yulExp "true" (YLit YulTrue),
      testCase "bare false parses as literal" $
        parsesAs yulExp "false" (YLit YulFalse)
    ]

-- Bug 4: pKeyword ran `string` (consuming input) before the
-- `notFollowedBy identChar` boundary check, and was not wrapped in `try`. In a
-- `choice`, a keyword parser that consumed a prefix and then failed the
-- boundary check aborted the whole choice instead of falling through to a
-- later alternative -- so a statement whose leading identifier merely starts
-- with a keyword (e.g. `format` vs `for`) failed to parse. Every
-- statement-level keyword is affected, including the merged
-- break/continue/leave.
statementKeywordTests :: TestTree
statementKeywordTests =
  testGroup
    "statement keyword boundary"
    ( [ testCase (kw ++ "-prefixed identifier parses as a call statement") $
          parsesAs yulStmt (kw ++ "ish") (YExp (yIdent (kw ++ "ish")))
      | kw <- ["let", "if", "for", "switch", "case", "default", "break", "continue", "leave"]
      ]
        ++
        -- The canonical case from the bug report: `format` must not be read as a
        -- `for` loop.
        [ testCase "`format` parses as an identifier, not a for loop" $
            parsesAs yulStmt "format" (YExp (yIdent "format")),
          -- Regression guards: the bare keyword statements still parse.
          testCase "bare break parses" $ parsesAs yulStmt "break" YBreak,
          testCase "bare continue parses" $ parsesAs yulStmt "continue" YContinue,
          testCase "bare leave parses" $ parsesAs yulStmt "leave" YLeave,
          testCase "genuine let still parses" $
            parsesAs yulStmt "let x" (YLet [Name "x"] Nothing),
          testCase "genuine if still parses" $
            parsesAs yulStmt "if cond { }" (YIf (yIdent "cond") []),
          testCase "genuine for still parses" $
            parsesAs yulStmt "for { } cond { } { }" (YFor [] (yIdent "cond") [] [])
        ]
    )

-- Bug 3: YSwitch used `many yulCase`, so a switch with zero `case` clauses
-- parsed successfully. Yul requires at least one case.
switchTests :: TestTree
switchTests =
  testGroup
    "switch requires a case"
    [ testCase "switch with no cases (default only) is rejected" $
        parseFails yulStmt "switch x default { }",
      -- Regression guard: a switch with a case still parses.
      testCase "switch with a case parses" $
        parsesAs
          yulStmt
          "switch x case 1 { }"
          (YSwitch (yIdent "x") [(YulNumber 1, [])] Nothing)
    ]
