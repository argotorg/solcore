module NewSyntaxDeclTests (newSyntaxDeclTests) where

import Common.LightYear (Parser, eof, runParserE)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Solcore.Diagnostics (Diagnostic (..), Label (..), LabelStyle (..), SourceSpan (..), compilerErrorDiagnostics, compilerErrorText)
import Solcore.Frontend.Lexer.SolcoreLexer (sc)
import Solcore.Frontend.Parser.Decl (compUnitP, importP, topDeclP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Frontend.Syntax.SyntaxTree
import Solcore.Pipeline.Options (Option (..), emptyOption)
import Solcore.Pipeline.SolcorePipeline (compile)
import Test.Tasty
import Test.Tasty.HUnit

newSyntaxDeclTests :: TestTree
newSyntaxDeclTests =
  testGroup
    "new syntax declarations"
    [ testCase "nullary function callback compiles through specialization" $ do
        let folder = "test/new-syntax/functions"
            options = (emptyOption (folder ++ "/nullary.sol")) {optRootDir = folder, optNoGenDispatch = True}
        result <- compile options
        case result of
          Left err -> assertFailure err
          Right _ -> pure (),
      testGroup "array literals bind the defining standard library" $
        map
          (\file -> testCase file (compilesArrayFixture file))
          ["selective.sol", "namespace-alias.sol", "helper-alias.sol", "shadowed.sol", "implicit.sol"],
      testGroup "accepted declarations" (map accepts acceptedDeclarations),
      testGroup "rejected declarations" (map rejects rejectedDeclarations),
      testGroup "unsupported nested comptime types report their source" $
        map
          (\source -> testCase source (rejectsComptimePosition source))
          [ "enum Value { Value(comptime<word>) }",
            "enum Box<a> { Box(a) } function f(x: Box<comptime<word>>) {}",
            "type CompileTimeWord = comptime<word>;",
            "function f(callback: function(comptime<word>) returns (word)) {}",
            "function f(callback: function(word) returns (comptime<word>)) {}",
            "enum Box<a> {} type Nested = Box<comptime<word>>;",
            "function f() returns (comptime<comptime<word>>) {}"
          ],
      testCase "outer comptime parameter local and return modes still resolve" $ do
        unit <- parsedUnit "function f(comptime x: word) returns (comptime<word>) { let y: comptime<word> = x; return y; }"
        resolved <- nameResolution unit
        case resolved of
          Left err -> assertFailure (compilerErrorText err)
          Right _ -> pure (),
      testCase "wildcard import retains hiding" $
        parsesAs importP "import * from lib.foo hiding {bar, (+),};" $
          ImportOnly
            (LibraryPath (Name "foo"))
            (SelectItems [SelectAllItems] [Name "bar", Name "+"]),
      testCase "selected operator alias" $
        parsesAs importP "import {( + = ) as append, x,} from @vendor.api;" $
          ImportOnly
            (ExternalPath (Name "vendor") (Name "api"))
            (SelectItems [SelectItemAs (Name "+=") (Name "append"), SelectItem (Name "x")] []),
      testCase "empty enum constructor argument list" $
        parsesAs topDeclP "enum Unit { Unit() }" $
          TDataDef (DataTy (Name "Unit") [] [Constr (Name "Unit") []]),
      testCase "derive targets retained" $
        parsesAs topDeclP "#[derive(Eq, support.Show)] enum Unit { Unit }" $
          TDataDef
            ( withDataDerives [Name "Eq", QualName (Name "support") "Show"] $
                DataTy (Name "Unit") [] [Constr (Name "Unit") []]
            ),
      testCase "generic alias binders retained" $
        parsesAs topDeclP "type Id(a,) = a;" $
          TSym (TySym (Name "Id") [TyCon (Name "a") []] (TyCon (Name "a") [])),
      testCase "known pragma status" $
        parsesAs topDeclP "pragma no-coverage-condition Eq, Show,;" $
          TPragmaDecl (Pragma NoCoverageCondition (DisableFor (Name "Eq" :| [Name "Show"]))),
      testCase "unknown pragma retained" $
        parsesAs topDeclP "pragma custom-directive;" $
          TPragmaDecl (Pragma (CustomPragma "custom-directive") DisableAll),
      testCase "contract-local alias retained" $
        parsesAs topDeclP "contract C { type Word = word; }" $
          TContr (Contract (Name "C") [] [CSymDecl (TySym (Name "Word") [] (TyCon (Name "word") []))]),
      testCase "contract contextual field name" $
        parsesAs topDeclP "contract C { enum: word; }" $
          TContr (Contract (Name "C") [] [CFieldDecl (Field (Name "enum") (TyCon (Name "word") []) Nothing)]),
      testCase "interleaved imports" $
        parsesAs compUnitP "type A = word; import m; enum B {}" $
          CompUnit
            [ImportModule (RelativePath (Name "m"))]
            [TSym (TySym (Name "A") [] (TyCon (Name "word") [])), TDataDef (DataTy (Name "B") [] [])]
    ]
  where
    accepts source = testCase source (assertParses compUnitP source)
    rejects source = testCase source (assertRejects compUnitP source)

acceptedDeclarations :: [String]
acceptedDeclarations =
  [ "import * from foo;",
    "import * from foo hiding {bar, (+),};",
    "import * as foo from @vendor . api;",
    "import @vendor;",
    "import {foo, (+) as add,} from foo . bar hiding {baz,};",
    "export {*, foo, E(A, B), E(*), (+), module.path.*,};",
    "export module . path . {foo, E(*), (+), *,};",
    "export module.path.*;",
    "export module.path as alias;",
    "export {};",
    "export module.path.{};",
    "pragma no-coverage-condition;",
    "pragma no-patterson-condition Eq,;",
    "pragma no-bounded-variable-condition Eq, Show;",
    "pragma no-generic-instance-for;",
    "pragma no-generic-instance-for E,;",
    "pragma custom-directive Item,;",
    "type Word = word;",
    "type Word() = word;",
    "type Id(a,) = a;",
    "enum Empty {}",
    "enum Option<a,> { None(), Some(a), }",
    "#[derive(Eq, support.Show)] enum Unit { Unit }",
    "function f(a: word,) returns (word,) {}",
    "function f(comptime a: word) returns (comptime<word>) {}",
    "function f<a,>(a: a) returns (a) where (a: Eq,) {}",
    "function f() returns () {}",
    "function f() {}",
    "trait Eq<a,> { function eq(left: a, right: a,) returns (bool,); }",
    "trait Ord<a> where a: Eq { function cmp(left: a, right: a) returns (word); }",
    "impl Eq<word,> { function eq(left: word, right: word) returns (bool) {} }",
    "default impl<a,> Eq<a> where (a: Ord,) {}",
    "contract C<a,> { value: a; type Id(b,) = b; enum E { E() } constructor(x: word,) payable {} fallback() payable {} function f(x: word,) public payable returns (word,) {} }",
    "contract C { #[derive(Eq)] enum E { E } }",
    "contract C { fallback() {} }",
    "contract C { function f() public {} function g() payable {} function h() {} }"
  ]

rejectedDeclarations :: [String]
rejectedDeclarations =
  [ "import {* } from foo;",
    "import {foo, *} from foo;",
    "import {} from foo;",
    "import * as foo from bar hiding {baz};",
    "import foo as bar;",
    "import foo hiding {bar};",
    "import {E(*)} from foo;",
    "import {foo as (+)} from bar;",
    "export @vendor.api;",
    "export {@vendor.api.*};",
    "export {E()};",
    "export {E(A,)};",
    "export {foo as bar};",
    "pragma solidity ^0.8.0;",
    "pragma no-coverage-condition module.Eq;",
    "pragma no-generic-instance-for module.E;",
    "pragma no-coverage-condition {};",
    "alias Word = word;",
    "type Id<a> = a;",
    "type Word is word;",
    "data E = E;",
    "struct S { x: word; }",
    "interface I { function f() external; }",
    "library L {}",
    "enum Option<> { None }",
    "enum Pair<a> { Pair(a,) }",
    "#[derive()] enum E { E }",
    "#[derive(Eq,)] enum E { E }",
    "#[derive(Eq)] function f() {}",
    "#[derive(Eq)] #[derive(Show)] enum E { E }",
    "function f(x) {}",
    "function f(comptime x) {}",
    "function f(x: comptime<word>) {}",
    "function f() public {}",
    "function f() payable {}",
    "function f() pure {}",
    "function f() returns (x: word) {}",
    "function f() returns (comptime word) {}",
    "function f() -> word {}",
    "trait Eq {}",
    "trait module.Eq<a> {}",
    "trait Eq<a> { function f(x); }",
    "impl module.Eq<word> {}",
    "impl Eq<> {}",
    "constructor() {}",
    "fallback() {}",
    "contract C { fallback() external {} }",
    "contract C { fallback() public {} }",
    "contract C { fallback(x: word) {} }",
    "contract C { fallback() returns (word) {} }",
    "contract C { constructor() public {} }",
    "contract C { constructor(x) {} }",
    "contract C { function f() external {} }",
    "contract C { function f() internal {} }",
    "contract C { function f() private {} }",
    "contract C { function f() view {} }",
    "contract C { function f() payable public {} }",
    "contract C { function f() public public {} }",
    "contract C { function f() payable payable {} }",
    "contract C { function f() public; }",
    "contract C { struct S {} }"
  ]

assertParses :: (Show a) => Parser a -> String -> Assertion
assertParses parser source =
  case runParserE (sc *> parser <* eof) "<new-syntax>" source of
    Left err -> assertFailure err
    Right _ -> pure ()

assertRejects :: (Show a) => Parser a -> String -> Assertion
assertRejects parser source =
  case runParserE (sc *> parser <* eof) "<new-syntax>" source of
    Left _ -> pure ()
    Right parsed -> assertFailure ("Expected rejection, parsed " ++ show parsed)

parsesAs :: (Show a, Eq a) => Parser a -> String -> a -> Assertion
parsesAs parser source expected =
  case runParserE (sc *> parser <* eof) "<new-syntax>" source of
    Left err -> assertFailure err
    Right parsed -> assertEqual source expected parsed

compilesArrayFixture :: FilePath -> Assertion
compilesArrayFixture file = do
  let folder = "test/new-syntax/arrays"
      options = (emptyOption (folder ++ "/" ++ file)) {optRootDir = folder, optNoGenDispatch = True}
  result <- compile options
  case result of
    Left err -> assertFailure err
    Right _ -> pure ()

parsedUnit :: String -> IO CompUnit
parsedUnit source =
  case runParserE compUnitP "<comptime-position>" source of
    Left err -> assertFailure err >> pure (CompUnit [] [])
    Right unit -> pure unit

rejectsComptimePosition :: String -> Assertion
rejectsComptimePosition source = do
  -- Parse success is deliberate: these are valid reference syntax, while the
  -- semantic representation cannot yet implement their nested mode safely.
  unit <- parsedUnit source
  resolved <- nameResolution unit
  case resolved of
    Right _ -> assertFailure "Expected an explicit unsupported type-position diagnostic"
    Left err -> do
      assertBool (compilerErrorText err) ("comptime<T> is not supported in this type position" `isInfixOf` compilerErrorText err)
      let labels = concatMap diagnosticLabels (compilerErrorDiagnostics err)
          primarySpans = [labelSpan label | label <- labels, labelStyle label == Primary]
      assertBool "diagnostic points at the comptime wrapper" $
        any
          (\sourceSpan -> "comptime<" `isInfixOf` take (spanEndByte sourceSpan - spanStartByte sourceSpan) (drop (spanStartByte sourceSpan) source))
          primarySpans
