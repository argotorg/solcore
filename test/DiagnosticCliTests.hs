module DiagnosticCliTests where

import Data.List (isSuffixOf, stripPrefix)
import System.Directory (doesFileExist, findExecutable, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
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
            "note: module validation failed for <cwd>/test/diagnostics/undefined-name.solc"
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
            "note: module validation failed for <cwd>/test/diagnostics/duplicate-definition.solc",
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
            "note: module typecheck failed for <cwd>/test/diagnostics/type-mismatch.solc (no desugaring)"
          ],
      testCase "missing signature uses signature span" $
        expectFailure
          ["--root", "test/diagnostics", "--file", "test/diagnostics/missing-signature.solc", "--no-specialise"]
          [ "error[SC0220]: top-level function must have complete type annotations",
            "  --> <cwd>/test/diagnostics/missing-signature.solc:1:10",
            "  |",
            "1 | function foo() {",
            "  |          ^^^ incomplete signature",
            "note: signature: function foo ()",
            "note: module typecheck failed for <cwd>/test/diagnostics/missing-signature.solc (no desugaring)",
            "help: annotate every parameter (name : Type) and provide a return type (-> Type)"
          ],
      testCase "polymorphic type error uses signature span" $
        expectFailure
          ["--root", "test/diagnostics", "--file", "test/diagnostics/not-polymorphic-enough.solc", "--no-specialise"]
          [ "error[SC0209]: type is not polymorphic enough",
            "  --> <cwd>/test/diagnostics/not-polymorphic-enough.solc:1:21",
            "  |",
            "1 | forall a . function fromWord(x : word) -> a {",
            "  |                     ^^^^^^^^ annotated type is not polymorphic enough",
            "note: annotated type: forall a . word -> a",
            "note: inferred type: word -> word",
            "note: in: forall a . function fromWord (x : word) -> a",
            "note: in: forall a . function fromWord (x : word) -> a {",
            "      let result ;",
            "      assembly {",
            "      result := x",
            "      }",
            "      return result;",
            "      }",
            "note: module typecheck failed for <cwd>/test/diagnostics/not-polymorphic-enough.solc (no desugaring)"
          ],
      testCase "missing instance" $
        expectFailure
          ["--root", "test/examples/cases", "--file", "test/examples/cases/missing-instance.solc", "--no-specialise"]
          [ "error[SC0223]: cannot entail: word : Typedef (word)",
            "  --> <cwd>/test/examples/cases/missing-instance.solc:12:14",
            "   |",
            "12 |     function load(ptr:word) -> word {",
            "   |              ^^^^ unsolved constraint",
            "note: using defined instances:",
            "note: in: function load (ptr : word) -> word {",
            "      return Typedef.abs(MemoryType.load(ptr) : word);",
            "      }",
            "note: in: instance word : MemoryType {",
            "      function load (ptr : word) -> word {",
            "      return Typedef.abs(MemoryType.load(ptr) : word);",
            "      }",
            "      }",
            "note: module typecheck failed for <cwd>/test/examples/cases/missing-instance.solc (no desugaring)",
            "help: add a matching instance or strengthen the surrounding type context"
          ],
      testCase "dot shorthand constructor error" $
        expectFailure
          ["--root", "test/examples/cases", "--file", "test/examples/cases/dot-expression-unknown-fail.solc", "--no-specialise"]
          [ "error[SC0224]: no matching constructor for shorthand expression",
            "  --> <cwd>/test/examples/cases/dot-expression-unknown-fail.solc:4:11",
            "  |",
            "4 |   return .Nope(1);",
            "  |           ^^^^ shorthand constructor",
            "note: constructor: .Nope",
            "note: in: .Nope(1)",
            "note: in: function bad () -> Option {",
            "      return .Nope(1);",
            "      }",
            "note: module typecheck failed for <cwd>/test/examples/cases/dot-expression-unknown-fail.solc (no desugaring)",
            "help: use a constructor that is visible for the expected type"
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
      testCase "loader error before graph keeps source span" $
        expectFailure
          ["--root", "test/imports", "--file", "test/imports/external_lib_missing_fail.solc", "--no-specialise"]
          [ "error[SC0118]: external library root is not configured: @missing",
            "  --> <cwd>/test/imports/external_lib_missing_fail.solc:1:9",
            "  |",
            "1 | import @missing.math.api;",
            "  |         ^^^^^^^^^^^^^^^^ external library import",
            "note: <cwd>/test/imports/external_lib_missing_fail.solc",
            "note: import @missing.math.api",
            "help: pass --external-lib NAME=PATH for external imports"
          ],
      testCase "ambiguous selected import" $
        expectFailure
          ["--root", "test/imports", "--file", "test/imports/amb_main.solc", "--no-specialise"]
          [ "error[SC0120]: ambiguous selected imports",
            "  --> <cwd>/test/imports/amb_main.solc:2:14",
            "  |",
            "2 | import ambB.{pick};",
            "  |              ^^^^ ambiguous selected import",
            "note: pick imported from ambB, ambA",
            "help: use an explicit module qualifier or narrow the selected imports"
          ],
      testCase "hidden constructor import" $
        expectFailure
          ["--root", "test/imports", "--file", "test/imports/hidden_ctor_expr_fail.solc", "--no-specialise"]
          [ "error[SC0101]: undefined name: Err",
            "  --> <cwd>/test/imports/hidden_ctor_expr_fail.solc:4:16",
            "  |",
            "4 |   return Token.Err(0);",
            "  |                ^^^ unknown name",
            "note: in: return Token.Err(0) ;",
            "note: in: function main () -> Token {",
            "      return Token.Err(0) ;",
            "      }",
            "note: module validation failed for <cwd>/test/imports/hidden_ctor_expr_fail.solc"
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
  exe <- solCoreExecutable
  (exitCode, stdout, stderr) <- readProcessWithExitCode exe (stableDiagnosticArgs args) ""
  assertEqual "exit code" ExitSuccess exitCode
  assertEqual "stderr" "" stderr
  cwd <- normalizePath <$> getCurrentDirectory
  assertEqual "stdout" (unlinesNoTrailing expectedLines) (stripFinalNewline (normalizeOutput cwd stdout))

expectFailure :: [String] -> [String] -> Assertion
expectFailure args expectedLines = do
  exe <- solCoreExecutable
  (exitCode, stdout, stderr) <- readProcessWithExitCode exe (stableDiagnosticArgs args) ""
  assertEqual "exit code" (ExitFailure 1) exitCode
  assertEqual "stderr" "" stderr
  cwd <- normalizePath <$> getCurrentDirectory
  assertEqual "stdout" (unlinesNoTrailing expectedLines) (stripFinalNewline (normalizeOutput cwd stdout))

solCoreExecutable :: IO FilePath
solCoreExecutable = do
  envExe <- lookupEnv "SOL_CORE_EXE"
  pathExe <- findExecutable "sol-core"
  cwd <- getCurrentDirectory
  found <-
    firstExisting
      ( maybeToList envExe
          ++ maybeToList pathExe
          ++ [cwd </> "dist" </> "build" </> "sol-core" </> "sol-core"]
      )
  case found of
    Just exe -> pure exe
    Nothing -> assertFailure "could not find sol-core executable"

stableDiagnosticArgs :: [String] -> [String]
stableDiagnosticArgs args =
  args ++ ["--diagnostic-width", "240"]

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = pure Nothing
firstExisting (path : paths) = do
  exists <- doesFileExist path
  if exists then pure (Just path) else firstExisting paths

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]

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
