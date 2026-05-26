module DiagnosticCliTests where

import Data.List (isSuffixOf, stripPrefix)
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

diagnosticCliTests :: TestTree
diagnosticCliTests =
  testGroup
    "Diagnostic CLI snapshots"
    [ testCase "parser error" $
        expectFailure
          ["--root", "test/diagnostics", "--file", "test/diagnostics/parse-error.solc", "--no-specialise"]
          [ "error[SC0001]: parse error: unexpected TArrow",
            "  --> <cwd>/test/diagnostics/parse-error.solc:1:16",
            "  |",
            "1 | function main( -> word { return 0; }",
            "  |                ^^ unexpected token"
          ],
      testCase "undefined name" $
        expectFailure
          ["--root", "test/diagnostics", "--file", "test/diagnostics/undefined-name.solc", "--no-specialise"]
          [ "error[SC0101]: undefined name: missing",
            "  --> <cwd>/test/diagnostics/undefined-name.solc:1:34",
            "  |",
            "1 | function main() -> word { return missing; }",
            "  |                                  ^^^^^^^ unknown name",
            "note: in: return missing ;",
            "note: in: function main () -> word {",
            "      return missing ;",
            "      }",
            "note: module validation failed for",
            "      <cwd>/test/diagnostics/undefined-name.solc"
          ],
      testCase "duplicate definition" $
        expectFailure
          ["--root", "test/diagnostics", "--file", "test/diagnostics/duplicate-definition.solc", "--no-specialise"]
          [ "error[SC0108]: duplicate declarations in term namespace",
            "  --> <cwd>/test/diagnostics/duplicate-definition.solc:2:10",
            "  |",
            "1 | function foo() -> word { return 1; }",
            "  |          --- previous definition",
            "2 | function foo() -> word { return 2; }",
            "  |          ^^^ duplicate definition",
            "note: context: module",
            "note: foo",
            "note: module validation failed for",
            "      <cwd>/test/diagnostics/duplicate-definition.solc",
            "help: rename or remove the duplicate declaration"
          ],
      testCase "type mismatch" $
        expectFailure
          ["--root", "test/diagnostics", "--file", "test/diagnostics/type-mismatch.solc", "--no-specialise"]
          [ "error[SC0201]: types do not unify: bool and word",
            "  --> <cwd>/test/diagnostics/type-mismatch.solc:1:34",
            "  |",
            "1 | function main() -> word { return true; }",
            "  |                                  ^^^^ expression has mismatched type",
            "note: left type: bool",
            "note: right type: word",
            "note: in: true",
            "note: in: function main () -> word {",
            "      return true;",
            "      }",
            "note: module typecheck failed for",
            "      <cwd>/test/diagnostics/type-mismatch.solc (no desugaring)"
          ],
      testCase "legacy typecheck error has fallback span" $
        expectFailure
          ["--root", "test/diagnostics", "--file", "test/diagnostics/missing-signature.solc", "--no-specialise"]
          [ "error: module typecheck failed for <cwd>/test/diagnostics/missing-signature.solc (no desugaring):",
            "  --> <cwd>/test/diagnostics/missing-signature.solc:1:10",
            "  |",
            "1 | function foo() {",
            "  |          ^^^ diagnostic reported here",
            "note: Top-level function must have complete type annotations:",
            "note: function foo ()",
            "note: Annotate every parameter (name : Type) and provide a return type (-> Type)."
          ],
      testCase "import error" $
        expectFailure
          ["--root", "test/imports", "--file", "test/imports/select_unknown.solc", "--no-specialise"]
          [ "error[SC0110]: unknown import item",
            "  --> <cwd>/test/imports/select_unknown.solc:1:19",
            "  |",
            "1 | import selectlib.{missing};",
            "  |                   ^^^^^^^ unknown import item",
            "note: unknown selected imports:",
            "note: selectlib.missing",
            "help: check the imported module's exported names"
          ],
      testCase "short output" $
        expectFailure
          ["--root", "test/diagnostics", "--file", "test/diagnostics/undefined-name.solc", "--no-specialise", "--diagnostic-format", "short"]
          ["<cwd>/test/diagnostics/undefined-name.solc:1:34: error[SC0101]: undefined name: missing"],
      testCase "warnings always" $
        expectSuccess
          ["--root", "test/examples/cases", "--file", "test/examples/cases/redundant-match.solc", "--no-specialise", "--warnings", "always"]
          redundantWarningsSnapshot,
      testCase "warnings never" $
        expectSuccess
          ["--root", "test/examples/cases", "--file", "test/examples/cases/redundant-match.solc", "--no-specialise", "--warnings", "never"]
          [],
      testCase "warnings deny" $
        expectFailure
          ["--root", "test/examples/cases", "--file", "test/examples/cases/redundant-match.solc", "--no-specialise", "--warnings", "deny"]
          (map denyWarningLine redundantWarningsSnapshot)
    ]

expectSuccess :: [String] -> [String] -> Assertion
expectSuccess args expectedLines = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "sol-core" args ""
  assertEqual "exit code" ExitSuccess exitCode
  assertEqual "stderr" "" stderr
  cwd <- normalizePath <$> getCurrentDirectory
  assertEqual "stdout" (unlinesNoTrailing expectedLines) (stripFinalNewline (normalizeOutput cwd stdout))

expectFailure :: [String] -> [String] -> Assertion
expectFailure args expectedLines = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "sol-core" args ""
  assertEqual "exit code" (ExitFailure 1) exitCode
  assertEqual "stderr" "" stderr
  cwd <- normalizePath <$> getCurrentDirectory
  assertEqual "stdout" (unlinesNoTrailing expectedLines) (stripFinalNewline (normalizeOutput cwd stdout))

redundantWarningsSnapshot :: [String]
redundantWarningsSnapshot =
  [ "warning[SC0301]: redundant pattern clause",
    "  --> <cwd>/test/examples/cases/redundant-match.solc:6:7",
    "  |",
    "6 |     | Bool.True  => return Bool.True;",
    "  |       ^^^^^^^^^ redundant clause",
    "note: clause: | Bool.True<Bool> =>",
    "      return Bool.True<Bool>;",
    "note: in: match (x<Bool>)",
    "note: in: function f",
    "help: remove this clause or make an earlier pattern more specific",
    "",
    "warning[SC0301]: redundant pattern clause",
    "  --> <cwd>/test/examples/cases/redundant-match.solc:7:7",
    "  |",
    "7 |     | Bool.False => return Bool.False;",
    "  |       ^^^^^^^^^^ redundant clause",
    "note: clause: | Bool.False<Bool> =>",
    "      return Bool.False<Bool>;",
    "note: in: match (x<Bool>)",
    "note: in: function f",
    "help: remove this clause or make an earlier pattern more specific"
  ]

denyWarningLine :: String -> String
denyWarningLine "warning[SC0301]: redundant pattern clause" =
  "error[SC0301]: redundant pattern clause"
denyWarningLine "help: remove this clause or make an earlier pattern more specific" =
  "help: remove this clause or make an earlier pattern more specific\nhelp: pass --warnings=default, --warnings=always, or --warnings=never to allow this warning"
denyWarningLine line = line

normalizeOutput :: FilePath -> String -> String
normalizeOutput cwd =
  replace cwd "<cwd>" . normalizePath

normalizePath :: FilePath -> FilePath
normalizePath =
  map (\c -> if c == '\\' then '/' else c)

replace :: String -> String -> String -> String
replace needle replacement haystack
  | null needle = haystack
  | otherwise =
      case stripPrefix needle haystack of
        Just rest -> replacement ++ replace needle replacement rest
        Nothing ->
          case haystack of
            [] -> []
            c : rest -> c : replace needle replacement rest

stripFinalNewline :: String -> String
stripFinalNewline text
  | "\n" `isSuffixOf` text = take (length text - 1) text
  | otherwise = text

unlinesNoTrailing :: [String] -> String
unlinesNoTrailing [] = ""
unlinesNoTrailing [line] = line
unlinesNoTrailing (line : rest) = line ++ "\n" ++ unlinesNoTrailing rest
