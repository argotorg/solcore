module DiagnosticTests where

import Solcore.Diagnostics
import Test.Tasty
import Test.Tasty.HUnit

diagnosticTests :: TestTree
diagnosticTests =
  testGroup
    "Diagnostics"
    [ testCase "human diagnostic snapshot" test_humanDiagnosticSnapshot,
      testCase "short diagnostic snapshot" test_shortDiagnosticSnapshot,
      testCase "unicode diagnostic snapshot" test_unicodeDiagnosticSnapshot,
      testCase "diagnostic notes wrap to configured width" test_diagnosticWidthWrapsNotes,
      testCase "color always emits ANSI styles" test_colorAlwaysEmitsAnsi
    ]

test_humanDiagnosticSnapshot :: Assertion
test_humanDiagnosticSnapshot =
  renderDiagnostic defaultDiagnosticRenderOptions sourceMap undefinedNameDiagnostic
    @?= unlinesNoTrailing
      [ "error[SC0101]: undefined name `missing`",
        "  --> main.solc:2:10",
        "  |",
        "2 |   return missing;",
        "  |          ^^^^^^^ unknown name",
        "note: names must be in scope",
        "help: import it explicitly"
      ]

test_shortDiagnosticSnapshot :: Assertion
test_shortDiagnosticSnapshot =
  renderDiagnostic
    defaultDiagnosticRenderOptions {diagnosticFormat = DiagnosticShort}
    sourceMap
    undefinedNameDiagnostic
    @?= "main.solc:2:10: error[SC0101]: undefined name `missing`"

test_unicodeDiagnosticSnapshot :: Assertion
test_unicodeDiagnosticSnapshot =
  renderDiagnostic
    defaultDiagnosticRenderOptions {diagnosticUnicode = UnicodeAlways}
    sourceMap
    undefinedNameDiagnostic
    @?= unlinesNoTrailing
      [ "error[SC0101]: undefined name `missing`",
        "  ──> main.solc:2:10",
        "  │",
        "2 │   return missing;",
        "  │          ^^^^^^^ unknown name",
        "note: names must be in scope",
        "help: import it explicitly"
      ]

test_diagnosticWidthWrapsNotes :: Assertion
test_diagnosticWidthWrapsNotes =
  renderDiagnostic
    defaultDiagnosticRenderOptions {diagnosticWidth = 24}
    sourceMap
    ( undefinedNameDiagnostic
        { diagnosticNotes = ["alpha beta gamma delta epsilon"],
          diagnosticHelp = []
        }
    )
    @?= unlinesNoTrailing
      [ "error[SC0101]: undefined name `missing`",
        "  --> main.solc:2:10",
        "  |",
        "2 |   return missing;",
        "  |          ^^^^^^^ unknown name",
        "note: alpha beta gamma",
        "      delta epsilon"
      ]

test_colorAlwaysEmitsAnsi :: Assertion
test_colorAlwaysEmitsAnsi =
  renderDiagnostic
    defaultDiagnosticRenderOptions {diagnosticColor = ColorAlways, diagnosticFormat = DiagnosticShort}
    sourceMap
    undefinedNameDiagnostic
    @?= "main.solc:2:10: \ESC[1;31merror[SC0101]\ESC[0m: undefined name `missing`"

sourceMap :: SourceMap
sourceMap =
  sourceMapFromFiles [sourceFile]

sourceFile :: SourceFile
sourceFile =
  makeSourceFile "main.solc" (unlines ["function main() -> word {", "  return missing;", "}"])

undefinedNameDiagnostic :: Diagnostic
undefinedNameDiagnostic =
  Diagnostic
    { diagnosticSeverity = Error,
      diagnosticCode = Just (DiagnosticCode "SC0101"),
      diagnosticMessage = "undefined name `missing`",
      diagnosticLabels =
        [ Label
            { labelSpan =
                SourceSpan
                  { spanFile = "main.solc",
                    spanStartByte = 34,
                    spanEndByte = 41,
                    spanStartLine = 2,
                    spanStartColumn = 10,
                    spanEndLine = 2,
                    spanEndColumn = 17
                  },
              labelStyle = Primary,
              labelMessage = Just "unknown name"
            }
        ],
      diagnosticNotes = ["names must be in scope"],
      diagnosticHelp = ["import it explicitly"]
    }

unlinesNoTrailing :: [String] -> String
unlinesNoTrailing [] = ""
unlinesNoTrailing [line] = line
unlinesNoTrailing (line : rest) = line ++ "\n" ++ unlinesNoTrailing rest
