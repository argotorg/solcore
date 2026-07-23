{-# LANGUAGE OverloadedStrings #-}

module ParserTests (parserTests) where

import Common.LightYear (Parser, runParserE)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Solcore.Frontend.Lexer.SolcoreLexer (sc)
import Solcore.Frontend.Parser.Decl (importP, topDeclP)
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.Patterns (patP)
import Solcore.Frontend.Parser.SolcoreTypes (predP, typeP)
import Solcore.Frontend.Parser.Stmt (bodyP, stmtP)
import Solcore.Frontend.Pretty.TreePretty qualified as TreePretty
import Solcore.Frontend.Syntax.Contract qualified as Resolved
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Frontend.Syntax.SyntaxTree
import Solcore.Frontend.Syntax.Ty qualified as ResolvedTy
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (eof)

parsesAs :: (Show a, Eq a) => Parser a -> String -> a -> Assertion
parsesAs p src expected =
  case runParserE (sc *> p <* eof) "<test>" src of
    Left err -> assertFailure ("Parse error:\n" ++ err)
    Right got -> assertEqual ("parsing: " ++ show src) expected got

parseFails :: (Show a) => Parser a -> String -> Assertion
parseFails p src =
  case runParserE (sc *> p <* eof) "<test>" src of
    Left _ -> return ()
    Right got -> assertFailure ("Expected failure but parsed: " ++ show got)

nameResolutionFails :: String -> Assertion
nameResolutionFails src =
  case runParserE (sc *> topDeclP <* eof) "<test>" src of
    Left err -> assertFailure ("Parse error:\n" ++ err)
    Right parsed -> do
      resolved <- nameResolution (CompUnit [] [parsed])
      case resolved of
        Left _ -> pure ()
        Right got ->
          assertFailure
            ("Expected name-resolution failure but resolved: " ++ show got)

nameResolutionSucceeds :: String -> Assertion
nameResolutionSucceeds src =
  case runParserE (sc *> topDeclP <* eof) "<test>" src of
    Left err -> assertFailure ("Parse error:\n" ++ err)
    Right parsed -> do
      resolved <- nameResolution (CompUnit [] [parsed])
      case resolved of
        Left err -> assertFailure ("Name resolution failed: " ++ show err)
        Right _ -> pure ()

roundTripsTopDecl :: String -> Assertion
roundTripsTopDecl src =
  case runParserE (sc *> topDeclP <* eof) "<test>" src of
    Left err -> assertFailure ("Initial parse error:\n" ++ err)
    Right parsed ->
      let rendered = TreePretty.pretty parsed
       in case runParserE (sc *> topDeclP <* eof) "<pretty>" rendered of
            Left err ->
              assertFailure
                ( "Pretty-printed declaration did not parse:\n"
                    ++ rendered
                    ++ "\n"
                    ++ err
                )
            Right reparsed ->
              assertEqual ("round trip: " ++ rendered) parsed reparsed

roundTripsStmt :: String -> Assertion
roundTripsStmt src =
  case runParserE (sc *> stmtP <* eof) "<test>" src of
    Left err -> assertFailure ("Initial parse error:\n" ++ err)
    Right parsed ->
      let rendered = TreePretty.pretty parsed
       in case runParserE (sc *> stmtP <* eof) "<pretty>" rendered of
            Left err ->
              assertFailure
                ( "Pretty-printed statement did not parse:\n"
                    ++ rendered
                    ++ "\n"
                    ++ err
                )
            Right reparsed ->
              assertEqual ("round trip: " ++ rendered) parsed reparsed

roundTripsType :: String -> Assertion
roundTripsType src =
  case runParserE (sc *> typeP <* eof) "<test>" src of
    Left err -> assertFailure ("Initial parse error:\n" ++ err)
    Right parsed ->
      let rendered = TreePretty.pretty parsed
       in case runParserE (sc *> typeP <* eof) "<pretty>" rendered of
            Left err ->
              assertFailure
                ( "Pretty-printed type did not parse:\n"
                    ++ rendered
                    ++ "\n"
                    ++ err
                )
            Right reparsed ->
              assertEqual ("round trip: " ++ rendered) parsed reparsed

expP :: Parser Exp
expP = exprP bodyP

parserTests :: TestTree
parserTests =
  testGroup
    "Parser"
    [ typeTests,
      predTests,
      patternTests,
      exprTests,
      stmtTests,
      declTests,
      importTests,
      pragmaTests,
      declarationShellTests,
      keywordPrefixTests,
      legacySyntaxTests
    ]

word :: Ty
word = TyCon "word" []

bool :: Ty
bool = TyCon "bool" []

typeTests :: TestTree
typeTests =
  testGroup
    "Types"
    [ testCase "simple named type" $
        parsesAs typeP "word" word,
      testCase "generic type" $
        parsesAs typeP "Option<word>" (TyCon "Option" [word]),
      testCase "generic type with two arguments" $
        parsesAs typeP "Result<word, bool>" (TyCon "Result" [word, bool]),
      testCase "qualified generic type" $
        parsesAs
          typeP
          "pkg.Result<word, Error>"
          (TyCon (QualName "pkg" "Result") [word, TyCon "Error" []]),
      testCase "mapping type" $
        parsesAs
          typeP
          "mapping(address => word)"
          (TyCon "mapping" [TyCon "address" [], word]),
      testCase "dynamic array type" $
        parsesAs typeP "word[]" (TyCon "array" [word]),
      testCase "nested dynamic array type" $
        parsesAs typeP "word[][]" (TyCon "array" [TyCon "array" [word]]),
      testCase "fixed array stores size before element type" $
        parsesAs typeP "word[4]" (TyCon "array" [TyCon "4" [], word]),
      testCase "fixed array accepts a type-level size" $
        parsesAs typeP "word[N]" (TyCon "array" [TyCon "N" [], word]),
      testCase "data location follows the complete array type" $
        parsesAs
          typeP
          "word[] storage"
          (TyCon "storage" [TyCon "array" [word]]),
      testCase "function type" $
        parsesAs
          typeP
          "function(word) internal returns (bool)"
          (FunctionTy [word] (Just FunctionTypeInternal) (Just [bool])),
      testCase "multi-parameter function type retains external visibility" $
        parsesAs
          typeP
          "function(word, bool) external returns (word)"
          (FunctionTy [word, bool] (Just FunctionTypeExternal) (Just [word])),
      testCase "zero-arity function type remains distinct in the source AST" $
        parsesAs
          typeP
          "function() internal returns (word)"
          (FunctionTy [] (Just FunctionTypeInternal) (Just [word])),
      testCase "function type preserves an omitted visibility and returns clause" $
        parsesAs
          typeP
          "function()"
          (FunctionTy [] Nothing Nothing),
      testCase "function type preserves multiple return items" $
        parsesAs
          typeP
          "function() external returns (word, bool)"
          (FunctionTy [] (Just FunctionTypeExternal) (Just [word, bool])),
      testCase "function type accepts an array suffix" $
        parsesAs
          typeP
          "function(word) internal returns (bool)[]"
          (TyCon "array" [FunctionTy [word] (Just FunctionTypeInternal) (Just [bool])]),
      testCase "function type syntax survives source pretty-printing" $
        mapM_
          roundTripsType
          [ "function() internal returns (word)",
            "function(word, bool) external returns (word, bool)",
            "function()"
          ],
      testCase "unit type" $
        parsesAs typeP "()" (TyCon "()" []),
      testCase "parenthesized single type" $
        parsesAs typeP "(word)" word,
      testCase "pair type in parens" $
        parsesAs typeP "(word, bool)" (pairTy word bool),
      testCase "triple type in parens" $
        parsesAs typeP "(word, bool, word)" (pairTy word (pairTy bool word)),
      testCase "qualified name in type" $
        parsesAs typeP "Foo.Bar" (TyCon (QualName "Foo" "Bar") []),
      -- Failure cases
      testCase "unclosed paren fails" $
        parseFails typeP "(word",
      testCase "unclosed generic argument list fails" $
        parseFails typeP "Option<word"
    ]

predTests :: TestTree
predTests =
  testGroup
    "Predicates"
    [ testCase "simple predicate" $
        parsesAs predP "t:Eq" (InCls "Eq" (TyCon "t" []) []),
      testCase "qualified class name" $
        parsesAs predP "t:Foo.Eq" (InCls (QualName "Foo" "Eq") (TyCon "t" []) []),
      testCase "predicate with one param" $
        parsesAs predP "t:Functor<word>" (InCls "Functor" (TyCon "t" []) [word]),
      testCase "predicate with two params" $
        parsesAs predP "t:Bifunctor<word,bool>" (InCls "Bifunctor" (TyCon "t" []) [word, bool]),
      testCase "compound main type" $
        parsesAs predP "(word,bool):Pair" (InCls "Pair" (pairTy word bool) [])
    ]

patternTests :: TestTree
patternTests =
  testGroup
    "Patterns"
    [ testCase "wildcard" $
        parsesAs patP "_" PWildcard,
      testCase "integer literal" $
        parsesAs patP "42" (PLit (IntLit 42)),
      testCase "string literal" $
        parsesAs patP "\"hi\"" (PLit (StrLit "hi")),
      testCase "constructor no args" $
        parsesAs patP "True" (Pat "True" []),
      testCase "constructor with one arg" $
        parsesAs patP "Some(x)" (Pat "Some" [Pat "x" []]),
      testCase "constructor with two args" $
        parsesAs patP "Pair(x,y)" (Pat "Pair" [Pat "x" [], Pat "y" []]),
      testCase "unit pattern" $
        parsesAs patP "()" (Pat "()" []),
      testCase "parenthesized single pattern" $
        parsesAs patP "(x)" (Pat "x" []),
      testCase "tuple pattern" $
        parsesAs patP "(x, y)" (Pat "pair" [Pat "x" [], Pat "y" []]),
      testCase "nested constructor" $
        parsesAs patP "Some(Pair(x,y))" (Pat "Some" [Pat "Pair" [Pat "x" [], Pat "y" []]]),
      testCase "dot pattern no args" $
        parsesAs patP ".None" (PatDot "None" []),
      testCase "dot pattern with args" $
        parsesAs patP ".Some(x)" (PatDot "Some" [Pat "x" []])
    ]

lit :: Integer -> Exp
lit = Lit . IntLit

var :: String -> Exp
var n = ExpVar Nothing (Name n)

unitExp :: Exp
unitExp = ExpName Nothing "()" []

exprTests :: TestTree
exprTests =
  testGroup
    "Expressions"
    [ testCase "integer literal" $
        parsesAs expP "42" (lit 42),
      testCase "zero literal" $
        parsesAs expP "0" (lit 0),
      testCase "string literal" $
        parsesAs expP "\"hello\"" (Lit (StrLit "hello")),
      testCase "variable" $
        parsesAs expP "x" (var "x"),
      testCase "nullary call" $
        parsesAs expP "f()" (ExpName Nothing "f" []),
      testCase "unary call" $
        parsesAs expP "f(1)" (ExpName Nothing "f" [lit 1]),
      testCase "binary call" $
        parsesAs expP "f(1, 2)" (ExpName Nothing "f" [lit 1, lit 2]),
      testCase "addition" $
        parsesAs expP "1 + 2" (ExpPlus (lit 1) (lit 2)),
      testCase "subtraction" $
        parsesAs expP "3 - 1" (ExpMinus (lit 3) (lit 1)),
      testCase "multiplication" $
        parsesAs expP "2 * 3" (ExpTimes (lit 2) (lit 3)),
      testCase "division" $
        parsesAs expP "6 / 2" (ExpDivide (lit 6) (lit 2)),
      testCase "modulo" $
        parsesAs expP "5 % 3" (ExpModulo (lit 5) (lit 3)),
      testCase "exponentiation is right-associative" $
        parsesAs
          expP
          "2 ** 3 ** 4"
          (ExpPower (lit 2) (ExpPower (lit 3) (lit 4))),
      testCase "mul binds tighter than add" $
        parsesAs expP "1 + 2 * 3" (ExpPlus (lit 1) (ExpTimes (lit 2) (lit 3))),
      testCase "add then mul" $
        parsesAs expP "1 * 2 + 3" (ExpPlus (ExpTimes (lit 1) (lit 2)) (lit 3)),
      testCase "subtraction is left-associative" $
        parsesAs expP "3 - 2 - 1" (ExpMinus (ExpMinus (lit 3) (lit 2)) (lit 1)),
      testCase "addition binds tighter than left shift" $
        parsesAs
          expP
          "x + y << n"
          (ExpShiftL (ExpPlus (var "x") (var "y")) (var "n")),
      testCase "addition on the right binds tighter than left shift" $
        parsesAs
          expP
          "x << n + 1"
          (ExpShiftL (var "x") (ExpPlus (var "n") (lit 1))),
      testCase "right shift" $
        parsesAs expP "x >> n" (ExpShiftR (var "x") (var "n")),
      testCase "less-than" $
        parsesAs expP "x < y" (ExpLT (var "x") (var "y")),
      testCase "greater-than" $
        parsesAs expP "x > y" (ExpGT (var "x") (var "y")),
      testCase "less-than-or-equal" $
        parsesAs expP "x <= y" (ExpLE (var "x") (var "y")),
      testCase "greater-than-or-equal" $
        parsesAs expP "x >= y" (ExpGE (var "x") (var "y")),
      testCase "equality" $
        parsesAs expP "x == y" (ExpEE (var "x") (var "y")),
      testCase "inequality" $
        parsesAs expP "x != y" (ExpNE (var "x") (var "y")),
      testCase "arith tighter than comparison" $
        parsesAs
          expP
          "a + b == c + d"
          (ExpEE (ExpPlus (var "a") (var "b")) (ExpPlus (var "c") (var "d"))),
      testCase "logical and" $
        parsesAs expP "x && y" (ExpLAnd (var "x") (var "y")),
      testCase "logical or" $
        parsesAs expP "x || y" (ExpLOr (var "x") (var "y")),
      testCase "logical not" $
        parsesAs expP "!x" (ExpLNot (var "x")),
      testCase "and binds tighter than or" $
        parsesAs expP "a || b && c" (ExpLOr (var "a") (ExpLAnd (var "b") (var "c"))),
      testCase "comparison tighter than and" $
        parsesAs
          expP
          "a < b && c > d"
          (ExpLAnd (ExpLT (var "a") (var "b")) (ExpGT (var "c") (var "d"))),
      testCase "ternary operator" $
        parsesAs expP "x ? 1 : 2" (ExpCond (var "x") (lit 1) (lit 2)),
      testCase "explicit conversion" $
        parsesAs expP "x as word" (TyExp (var "x") word),
      testCase "conversion accepts a qualified generic target" $
        parsesAs
          expP
          "x as pkg.Result<word, bool>"
          (TyExp (var "x") (TyCon (QualName "pkg" "Result") [word, bool])),
      testCase "conversion is left-associative" $
        parsesAs
          expP
          "x as word as bool"
          (TyExp (TyExp (var "x") word) bool),
      testCase "conversion binds tighter than addition" $
        parsesAs
          expP
          "x as word + y"
          (ExpPlus (TyExp (var "x") word) (var "y")),
      testCase "parentheses allow converting a complete addition" $
        parsesAs
          expP
          "(x + y) as word"
          (TyExp (ExpPlus (var "x") (var "y")) word),
      testCase "conversion is accepted in both ternary branches" $
        parsesAs
          expP
          "condition ? x as word : y as bool"
          (ExpCond (var "condition") (TyExp (var "x") word) (TyExp (var "y") bool)),
      testCase "function-style syntax remains an ordinary call" $
        parsesAs expP "word(x)" (ExpName Nothing "word" [var "x"]),
      testCase "field access" $
        parsesAs expP "x.foo" (ExpVar (Just (var "x")) "foo"),
      testCase "method call" $
        parsesAs expP "x.foo(1)" (ExpName (Just (var "x")) "foo" [lit 1]),
      testCase "chained field access" $
        parsesAs expP "x.y.z" (ExpVar (Just (ExpVar (Just (var "x")) "y")) "z"),
      testCase "index expression" $
        parsesAs expP "arr[0]" (ExpIndexed (var "arr") (lit 0)),
      testCase "chained index" $
        parsesAs expP "m[i][j]" (ExpIndexed (ExpIndexed (var "m") (var "i")) (var "j")),
      testCase "unit expression" $
        parsesAs expP "()" (ExpName Nothing "()" []),
      testCase "parenthesized expression" $
        parsesAs expP "(x)" (var "x"),
      testCase "pair expression" $
        parsesAs
          expP
          "(a, b)"
          (ExpName Nothing "pair" [var "a", var "b"]),
      testCase "triple expression right-folds" $
        parsesAs
          expP
          "(a, b, c)"
          (ExpName Nothing "pair" [var "a", ExpName Nothing "pair" [var "b", var "c"]]),
      testCase "dot name without args" $
        parsesAs expP ".None" (ExpDotName "None" []),
      testCase "dot name with args" $
        parsesAs expP ".Some(1)" (ExpDotName "Some" [lit 1]),
      testCase "lambda no params" $
        parsesAs
          expP
          "lam() returns (word) { return 0; }"
          (Lam [] [Return (lit 0)] (Just word)),
      testCase "lambda with typed param" $
        parsesAs
          expP
          "lam(x:word) returns (word) { return x; }"
          (Lam [Typed False "x" word] [Return (var "x")] (Just word)),
      testCase "lambda without return type" $
        parsesAs
          expP
          "lam(x:word) { return x; }"
          (Lam [Typed False "x" word] [Return (var "x")] Nothing)
    ]

-- | Identifiers that start with a keyword (e.g. `enumValue`, which begins with
-- `enum`) must not be mistaken for the keyword. The lexer's `keyword` parser is
-- atomic, so a keyword tried as an alternative backtracks instead of consuming
-- the prefix.
keywordPrefixTests :: TestTree
keywordPrefixTests =
  testGroup
    "Keyword prefixes"
    [ testCase "statement-initial assignment to keyword-prefixed name" $
        parsesAs stmtP "enumValue = 2;" (Assign (var "enumValue") (lit 2)),
      testCase "statement-initial expression with keyword-prefixed name" $
        parsesAs stmtP "returnsValue;" (StmtExp (var "returnsValue")),
      testCase "contract field with keyword-prefixed name" $
        parsesAs
          topDeclP
          "contract C { traitValue : word; }"
          (TContr (Contract "C" [] [CFieldDecl (Field "traitValue" word Nothing)]))
    ]

stmtTests :: TestTree
stmtTests =
  testGroup
    "Statements"
    [ testCase "let no type no init" $
        parsesAs stmtP "let x;" (Let False "x" Nothing Nothing),
      testCase "let with type" $
        parsesAs stmtP "let x : word;" (Let False "x" (Just word) Nothing),
      testCase "let with init" $
        parsesAs stmtP "let x = 42;" (Let False "x" Nothing (Just (lit 42))),
      testCase "let with type and init" $
        parsesAs stmtP "let x : word = 42;" (Let False "x" (Just word) (Just (lit 42))),
      testCase "comptime let binding" $
        parsesAs stmtP "let comptime x : word = 42;" (Let True "x" (Just word) (Just (lit 42))),
      testCase "typed tuple destructuring let" $
        parsesAs
          stmtP
          "let (amount, ok): (word, bool) = readResult();"
          ( LetPattern
              False
              (Pat "pair" [Pat "amount" [], Pat "ok" []])
              (Just (TyCon "pair" [word, bool]))
              (ExpName Nothing "readResult" [])
          ),
      testCase "untyped nested tuple destructuring let" $
        parsesAs
          stmtP
          "let (left, (middle, right)) = readNested();"
          ( LetPattern
              False
              (Pat "pair" [Pat "left" [], Pat "pair" [Pat "middle" [], Pat "right" []]])
              Nothing
              (ExpName Nothing "readNested" [])
          ),
      testCase "tuple destructuring pretty-prints as new syntax" $
        roundTripsStmt "let (amount, (ok, fallbackValue)): (word, (bool, word)) = readResult();",
      testCase "tuple destructuring requires an initializer" $
        parseFails stmtP "let (left, right);",
      testCase "tuple destructuring rejects a singleton pattern" $
        parseFails stmtP "let (only) = readResult();",
      testCase "tuple destructuring rejects refutable constructor leaves" $
        parseFails stmtP "let (Some(value), rest) = readResult();",
      testCase "tuple destructuring rejects duplicate binders recursively" $
        parseFails stmtP "let (x, (y, x)) = readResult();",
      testCase "tuple destructuring allows repeated wildcards" $
        parsesAs
          stmtP
          "let (_, (_, x)) = readResult();"
          ( LetPattern
              False
              (Pat "pair" [PWildcard, Pat "pair" [PWildcard, Pat "x" []]])
              Nothing
              (ExpName Nothing "readResult" [])
          ),
      testCase "comptime tuple destructuring keeps its binding modifier" $
        parsesAs
          stmtP
          "let comptime (left, right) = readResult();"
          ( LetPattern
              True
              (Pat "pair" [Pat "left" [], Pat "right" []])
              Nothing
              (ExpName Nothing "readResult" [])
          ),
      testCase "comptime tuple destructuring pretty-prints as new syntax" $
        roundTripsStmt "let comptime (left, right): (word, word) = readResult();",
      testCase "return literal" $
        parsesAs stmtP "return 0;" (Return (lit 0)),
      testCase "return expression" $
        parsesAs stmtP "return x + 1;" (Return (ExpPlus (var "x") (lit 1))),
      testCase "bare return produces the unit expression" $
        parsesAs stmtP "return;" (Return unitExp),
      testCase "assignment" $
        parsesAs stmtP "x = 1;" (Assign (var "x") (lit 1)),
      testCase "plus-assign" $
        parsesAs stmtP "x += 1;" (StmtPlusEq (var "x") (lit 1)),
      testCase "minus-assign" $
        parsesAs stmtP "x -= 1;" (StmtMinusEq (var "x") (lit 1)),
      testCase "field assignment" $
        parsesAs
          stmtP
          "this.x = 1;"
          (Assign (ExpVar (Just (var "this")) "x") (lit 1)),
      testCase "call as statement requires semicolon" $
        parseFails stmtP "f()",
      testCase "call as statement with semicolon" $
        parsesAs stmtP "f();" (StmtExp (ExpName Nothing "f" [])),
      testCase "if without else" $
        parsesAs
          stmtP
          "if (x) { return 1; }"
          (If (var "x") [Return (lit 1)] []),
      testCase "if with else" $
        parsesAs
          stmtP
          "if (x) { return 1; } else { return 2; }"
          (If (var "x") [Return (lit 1)] [Return (lit 2)]),
      testCase "empty block" $
        parsesAs stmtP "{}" (Block []),
      testCase "block with statement" $
        parsesAs stmtP "{ let x = 1; }" (Block [Let False "x" Nothing (Just (lit 1))]),
      testCase "for loop" $
        parsesAs
          stmtP
          "for (let i = 0; i < 10; i = i + 1) { }"
          ( For
              (Let False "i" Nothing (Just (lit 0)))
              (ExpLT (var "i") (lit 10))
              (Assign (var "i") (ExpPlus (var "i") (lit 1)))
              []
          ),
      testCase "for loop with empty init and post" $
        parsesAs
          stmtP
          "for (; i < 10; ) { }"
          ( For
              EmptyStmt
              (ExpLT (var "i") (lit 10))
              EmptyStmt
              []
          ),
      testCase "for loop with empty init only" $
        parsesAs
          stmtP
          "for (; i < 10; i = i + 1) { }"
          ( For
              EmptyStmt
              (ExpLT (var "i") (lit 10))
              (Assign (var "i") (ExpPlus (var "i") (lit 1)))
              []
          ),
      testCase "for loop with empty post only" $
        parsesAs
          stmtP
          "for (let i = 0; i < 10; ) { }"
          ( For
              (Let False "i" Nothing (Just (lit 0)))
              (ExpLT (var "i") (lit 10))
              EmptyStmt
              []
          ),
      testCase "while loop remains distinct in the source AST" $
        parsesAs
          stmtP
          "while (condition) { continue; }"
          (While (var "condition") [Continue]),
      testCase "while loop survives source pretty-printing" $
        roundTripsStmt "while (condition) { continue; }",
      testCase "unchecked block remains distinct in the source AST" $
        parsesAs
          stmtP
          "unchecked { let x = 1; }"
          (Unchecked [Let False "x" Nothing (Just (lit 1))]),
      testCase "unchecked block survives source pretty-printing" $
        roundTripsStmt "unchecked { let x = 1; }",
      testCase "bare revert lowers to the revert operation" $
        parsesAs
          stmtP
          "revert;"
          (StmtExp (ExpName Nothing "revert" [])),
      testCase "match one equation" $
        parsesAs
          stmtP
          "match (x) { case 0 { return 1; } }"
          (Match [var "x"] [([PLit (IntLit 0)], [Return (lit 1)])]),
      testCase "match default arm" $
        parsesAs
          stmtP
          "match (x) { default { return 0; } }"
          (Match [var "x"] [([PWildcard], [Return (lit 0)])]),
      testCase "match constructor pattern" $
        parsesAs
          stmtP
          "match (x) { case Option.Some(v) { return v; } }"
          (Match [var "x"] [([Pat (QualName "Option" "Some") [Pat "v" []]], [Return (var "v")])]),
      testCase "match multiple equations" $
        parsesAs
          stmtP
          "match (x) { case 0 { return 0; } default { return 1; } }"
          ( Match
              [var "x"]
              [ ([PLit (IntLit 0)], [Return (lit 0)]),
                ([PWildcard], [Return (lit 1)])
              ]
          ),
      testCase "match multiple values" $
        parsesAs
          stmtP
          "match (x, y) { case (Some(a), Some(b)) { return a + b; } default { return 0; } }"
          ( Match
              [var "x", var "y"]
              [ ( [Pat "Some" [Pat "a" []], Pat "Some" [Pat "b" []]],
                  [Return (ExpPlus (var "a") (var "b"))]
                ),
                ([PWildcard, PWildcard], [Return (lit 0)])
              ]
          ),
      testCase "let without semicolon fails" $
        parseFails stmtP "let x"
    ]

declTests :: TestTree
declTests =
  testGroup
    "Declarations"
    [ testCase "nullary function" $
        parsesAs
          topDeclP
          "function answer() returns (word) { return 42; }"
          ( TFunDef
              ( FunDef
                  False
                  (Signature [] [] "answer" [] False (Just word) False)
                  [Return (lit 42)]
              )
          ),
      testCase "unary function" $
        parsesAs
          topDeclP
          "function id(x:word) returns (word) { return x; }"
          ( TFunDef
              ( FunDef
                  False
                  (Signature [] [] "id" [Typed False "x" word] False (Just word) False)
                  [Return (var "x")]
              )
          ),
      testCase "named return lowers to its declared type" $
        parsesAs
          topDeclP
          "function namedResult() returns (result: word) { return 1; }"
          ( TFunDef
              ( FunDef
                  False
                  ( SignatureWithSyntax
                      []
                      []
                      "namedResult"
                      []
                      (Just [ReturnItem False (Just "result") word])
                      []
                  )
                  [Return (lit 1)]
              )
          ),
      testCase "comptime parameter and return are recorded in the signature" $
        parsesAs
          topDeclP
          "function staged(comptime x:word) returns (comptime word) { return x; }"
          ( TFunDef
              ( FunDef
                  False
                  (Signature [] [] "staged" [Typed True "x" word] True (Just word) False)
                  [Return (var "x")]
              )
          ),
      testCase "multiple return types fold into the tuple AST" $
        parsesAs
          topDeclP
          "function pairValue() returns (word, bool) { return (1, true); }"
          ( TFunDef
              ( FunDef
                  False
                  (Signature [] [] "pairValue" [] False (Just (pairTy word bool)) False)
                  [Return (ExpName Nothing "pair" [lit 1, var "true"])]
              )
          ),
      testCase "named return items survive source pretty-printing" $
        roundTripsTopDecl
          "function namedPair() returns (left: word, comptime right: bool) { return (1, true); }",
      testCase "polymorphic function" $
        parsesAs
          topDeclP
          "function id<a>(x:a) returns (a) { return x; }"
          ( TFunDef
              ( FunDef
                  False
                  ( Signature
                      [TyCon "a" []]
                      []
                      "id"
                      [Typed False "x" (TyCon "a" [])]
                      False
                      (Just (TyCon "a" []))
                      False
                  )
                  [Return (var "x")]
              )
          ),
      testCase "constrained function" $
        parsesAs
          topDeclP
          "function eqSelf<a>(x:a) returns (bool) where a:Eq { return x == x; }"
          ( TFunDef
              ( FunDef
                  False
                  ( Signature
                      [TyCon "a" []]
                      [InCls "Eq" (TyCon "a" []) []]
                      "eqSelf"
                      [Typed False "x" (TyCon "a" [])]
                      False
                      (Just bool)
                      False
                  )
                  [Return (ExpEE (var "x") (var "x"))]
              )
          ),
      testCase "legacy declaration word is reusable as an identifier" $
        parsesAs
          topDeclP
          "function data() returns (()) { return; }"
          ( TFunDef
              ( FunDef
                  False
                  (Signature [] [] "data" [] False (Just (TyCon "()" [])) False)
                  [Return unitExp]
              )
          ),
      testCase "empty enum" $
        parsesAs
          topDeclP
          "enum Void { }"
          (TDataDef (DataTy "Void" [] [])),
      testCase "enum with nullary constructors" $
        parsesAs
          topDeclP
          "enum Bool { True, False }"
          (TDataDef (DataTy "Bool" [] [Constr "True" [], Constr "False" []])),
      testCase "data location word remains available as a value constructor name" $
        parsesAs
          topDeclP
          "enum Location { storage }"
          (TDataDef (DataTy "Location" [] [Constr "storage" []])),
      testCase "generic enum with payload constructor" $
        parsesAs
          topDeclP
          "enum Option<a> { Some(a), None }"
          ( TDataDef
              ( DataTy
                  "Option"
                  [TyCon "a" []]
                  [Constr "Some" [TyCon "a" []], Constr "None" []]
              )
          ),
      testCase "user-defined value type" $
        parsesAs
          topDeclP
          "type Word is word;"
          (TSym (TySym "Word" [] word)),
      testCase "generic user-defined value type" $
        parsesAs
          topDeclP
          "type Pair<a, b> is (a, b);"
          ( TSym
              ( TySym
                  "Pair"
                  [TyCon "a" [], TyCon "b" []]
                  (pairTy (TyCon "a" []) (TyCon "b" []))
              )
          ),
      testCase "trait with one method" $
        parsesAs
          topDeclP
          "trait Eq<a> { function eq(x:a, y:a) returns (bool); }"
          ( TClassDef
              ( Class
                  [TyCon "a" []]
                  []
                  "Eq"
                  []
                  (TyCon "a" [])
                  [ Signature
                      []
                      []
                      "eq"
                      [Typed False "x" (TyCon "a" []), Typed False "y" (TyCon "a" [])]
                      False
                      (Just bool)
                      False
                  ]
              )
          ),
      testCase "trait with where clause" $
        parsesAs
          topDeclP
          "trait Ord<a> where a:Eq { function cmp(x:a, y:a) returns (word); }"
          ( TClassDef
              ( Class
                  [TyCon "a" []]
                  [InCls "Eq" (TyCon "a" []) []]
                  "Ord"
                  []
                  (TyCon "a" [])
                  [ Signature
                      []
                      []
                      "cmp"
                      [Typed False "x" (TyCon "a" []), Typed False "y" (TyCon "a" [])]
                      False
                      (Just word)
                      False
                  ]
              )
          ),
      testCase "impl with one method" $
        parsesAs
          topDeclP
          "impl Eq<word> { function eq(x:word, y:word) returns (bool) { return x == y; } }"
          ( TInstDef
              ( Instance
                  False
                  []
                  []
                  "Eq"
                  []
                  word
                  [ FunDef
                      False
                      (Signature [] [] "eq" [Typed False "x" word, Typed False "y" word] False (Just bool) False)
                      [Return (ExpEE (var "x") (var "y"))]
                  ]
              )
          ),
      testCase "default impl records its default status" $
        parsesAs
          topDeclP
          "default impl Eq<word> { function eq(x:word, y:word) returns (bool) { return x == y; } }"
          ( TInstDef
              ( Instance
                  True
                  []
                  []
                  "Eq"
                  []
                  word
                  [ FunDef
                      False
                      (Signature [] [] "eq" [Typed False "x" word, Typed False "y" word] False (Just bool) False)
                      [Return (ExpEE (var "x") (var "y"))]
                  ]
              )
          ),
      testCase "generic impl with where clause" $
        parsesAs
          topDeclP
          "impl<a> Eq<pair<a,a>> where a:Eq { function eq(x:pair<a,a>, y:pair<a,a>) returns (bool) { return 0; } }"
          ( TInstDef
              ( Instance
                  False
                  [TyCon "a" []]
                  [InCls "Eq" (TyCon "a" []) []]
                  "Eq"
                  []
                  (TyCon "pair" [TyCon "a" [], TyCon "a" []])
                  [ FunDef
                      False
                      ( Signature
                          []
                          []
                          "eq"
                          [ Typed False "x" (TyCon "pair" [TyCon "a" [], TyCon "a" []]),
                            Typed False "y" (TyCon "pair" [TyCon "a" [], TyCon "a" []])
                          ]
                          False
                          (Just bool)
                          False
                      )
                      [Return (lit 0)]
                  ]
              )
          ),
      testCase "empty contract" $
        parsesAs
          topDeclP
          "contract Empty { }"
          (TContr (Contract "Empty" [] [])),
      testCase "contract with field" $
        parsesAs
          topDeclP
          "contract C { x : word; }"
          (TContr (Contract "C" [] [CFieldDecl (Field "x" word Nothing)])),
      testCase "contract with initialized field" $
        parsesAs
          topDeclP
          "contract C { x : word = 0; }"
          (TContr (Contract "C" [] [CFieldDecl (Field "x" word (Just (lit 0)))])),
      testCase "contract with function" $
        parsesAs
          topDeclP
          "contract C { function get() returns (word) { return x; } }"
          ( TContr
              ( Contract
                  "C"
                  []
                  [ CFunDecl
                      ( FunDef
                          False
                          (Signature [] [] "get" [] False (Just word) False)
                          [Return (var "x")]
                      )
                  ]
              )
          ),
      testCase "contract with public function" $
        parsesAs
          topDeclP
          "contract C { function get() public returns (word) { return x; } }"
          ( TContr
              ( Contract
                  "C"
                  []
                  [ CFunDecl
                      ( FunDef
                          True
                          ( SignatureWithSyntax
                              []
                              []
                              "get"
                              []
                              (Just [ReturnItem False Nothing word])
                              [VisibilityModifier VisibilityPublic]
                          )
                          [Return (var "x")]
                      )
                  ]
              )
          ),
      testCase "contract with public payable function" $
        parsesAs
          topDeclP
          "contract C { function pay() public payable returns (word) { return 0; } }"
          ( TContr
              ( Contract
                  "C"
                  []
                  [ CFunDecl
                      ( FunDef
                          True
                          ( SignatureWithSyntax
                              []
                              []
                              "pay"
                              []
                              (Just [ReturnItem False Nothing word])
                              [ VisibilityModifier VisibilityPublic,
                                MutabilityModifier MutabilityPayable
                              ]
                          )
                          [Return (lit 0)]
                      )
                  ]
              )
          ),
      testCase "contract functions accept pure, view, private, internal, and external modifiers" $
        parsesAs
          topDeclP
          ( "contract C {"
              ++ " function pureFn() pure { return; }"
              ++ " function viewFn() view { return; }"
              ++ " function privateFn() private { return; }"
              ++ " function internalFn() internal { return; }"
              ++ " function externalFn() external { return; }"
              ++ " }"
          )
          ( TContr
              ( Contract
                  "C"
                  []
                  [ CFunDecl
                      ( FunDef
                          False
                          (SignatureWithSyntax [] [] "pureFn" [] Nothing [MutabilityModifier MutabilityPure])
                          [Return unitExp]
                      ),
                    CFunDecl
                      ( FunDef
                          False
                          (SignatureWithSyntax [] [] "viewFn" [] Nothing [MutabilityModifier MutabilityView])
                          [Return unitExp]
                      ),
                    CFunDecl
                      ( FunDef
                          False
                          (SignatureWithSyntax [] [] "privateFn" [] Nothing [VisibilityModifier VisibilityPrivate])
                          [Return unitExp]
                      ),
                    CFunDecl
                      ( FunDef
                          False
                          (SignatureWithSyntax [] [] "internalFn" [] Nothing [VisibilityModifier VisibilityInternal])
                          [Return unitExp]
                      ),
                    CFunDecl
                      ( FunDef
                          True
                          (SignatureWithSyntax [] [] "externalFn" [] Nothing [VisibilityModifier VisibilityExternal])
                          [Return unitExp]
                      )
                  ]
              )
          ),
      testCase "contract constructor" $
        parsesAs
          topDeclP
          "contract C { constructor(x:word) { return; } }"
          ( TContr
              ( Contract
                  "C"
                  []
                  [CConstrDecl (Constructor [Typed False "x" word] [Return unitExp] False)]
              )
          ),
      testCase "payable modifier follows constructor parameters" $
        parsesAs
          topDeclP
          "contract C { constructor(x:word) payable { return; } }"
          ( TContr
              ( Contract
                  "C"
                  []
                  [CConstrDecl (Constructor [Typed False "x" word] [Return unitExp] True)]
              )
          ),
      testCase "external payable fallback" $
        parsesAs
          topDeclP
          "contract C { fallback() external payable { return; } }"
          ( TContr
              ( Contract
                  "C"
                  []
                  [ CFunDecl
                      ( FunDef
                          False
                          ( SignatureWithSyntax
                              []
                              []
                              "fallback"
                              []
                              Nothing
                              [ VisibilityModifier VisibilityExternal,
                                MutabilityModifier MutabilityPayable
                              ]
                          )
                          [Return unitExp]
                      )
                  ]
              )
          ),
      testCase "fallback without external visibility fails" $
        parseFails
          topDeclP
          "contract C { fallback() { return; } }",
      testCase "multiple visibility modifiers fail" $
        parseFails
          topDeclP
          "contract C { function f() public external { return; } }",
      testCase "multiple mutability modifiers fail" $
        parseFails
          topDeclP
          "contract C { function f() pure view { return; } }",
      -- Contract visibility modifiers are not meaningful on impl methods.
      testCase "public instance method fails" $
        parseFails
          topDeclP
          "impl Eq<word> { function eq(x:word, y:word) public returns (bool) { return x == y; } }"
    ]

importTests :: TestTree
importTests =
  testGroup
    "Imports"
    [ testCase "dotted module import" $
        parsesAs
          importP
          "import std.dispatch;"
          (ImportModule (RelativePath (QualName "std" "dispatch"))),
      testCase "namespace alias import" $
        parsesAs
          importP
          "import * as dispatch from std.dispatch;"
          (ImportAlias (RelativePath (QualName "std" "dispatch")) "dispatch"),
      testCase "selective import" $
        parsesAs
          importP
          "import {address, uint256 as U256} from std;"
          ( ImportOnly
              (RelativePath "std")
              (SelectItems [SelectItem "address", SelectItemAs "uint256" "U256"] [])
          ),
      testCase "selective import from external module" $
        parsesAs
          importP
          "import {foo, bar as baz} from @ext.foo.bar;"
          ( ImportOnly
              (ExternalPath "ext" (QualName "foo" "bar"))
              (SelectItems [SelectItem "foo", SelectItemAs "bar" "baz"] [])
          )
    ]

pragmaTests :: TestTree
pragmaTests =
  testGroup
    "Pragmas"
    [ testCase "Solidity compatibility pragma retains its value" $
        parsesAs
          topDeclP
          "pragma solidity ^0.8.23;"
          (TPragmaDecl (Pragma (SolidityPragma "^0.8.23") Enabled)),
      testCase "ABI coder pragma retains its value" $
        parsesAs
          topDeclP
          "pragma abicoder v2;"
          (TPragmaDecl (Pragma (AbiCoderPragma "v2") Enabled)),
      testCase "disable coverage condition" $
        parsesAs
          topDeclP
          "pragma solcore noCoverageCondition;"
          (TPragmaDecl (Pragma NoCoverageCondition DisableAll)),
      testCase "disable Patterson condition" $
        parsesAs
          topDeclP
          "pragma solcore noPattersonCondition;"
          (TPragmaDecl (Pragma NoPattersonCondition DisableAll)),
      testCase "disable bound-variable condition" $
        parsesAs
          topDeclP
          "pragma solcore noBoundVariableCondition;"
          (TPragmaDecl (Pragma NoBoundVariableCondition DisableAll)),
      testCase "disable generic instance generation for a type" $
        parsesAs
          topDeclP
          "pragma solcore noGenericInstanceFor MyType;"
          (TPragmaDecl (Pragma NoGenericInstanceFor (DisableFor ("MyType" :| []))))
    ]

legacySyntaxTests :: TestTree
legacySyntaxTests =
  testGroup
    "Legacy syntax is rejected"
    [ testCase "parenthesized generic type arguments" $
        parseFails typeP "pair(word, bool)",
      testCase "arrow function type" $
        parseFails typeP "word -> bool",
      testCase "at-sign proxy type" $
        parseFails typeP "@word",
      testCase "at-sign proxy expression" $
        parseFails expP "@word",
      testCase "expression colon annotation" $
        parseFails expP "x : word",
      testCase "arrow function return" $
        parseFails topDeclP "function answer() -> word { return 42; }",
      testCase "forall generic prefix" $
        parseFails topDeclP "forall a. function id(x:a) returns (a) { return x; }",
      testCase "data declaration" $
        parseFails topDeclP "data Bool = True | False;",
      testCase "class declaration" $
        parseFails topDeclP "forall a. class a:Eq { function eq(x:a, y:a) -> bool; }",
      testCase "instance declaration" $
        parseFails topDeclP "instance word:Eq { function eq(x:word, y:word) -> bool { return x == y; } }",
      testCase "leading public modifier" $
        parseFails
          topDeclP
          "contract C { public function get() returns (word) { return x; } }",
      testCase "leading payable constructor modifier" $
        parseFails
          topDeclP
          "contract C { payable constructor(x:word) { return; } }",
      testCase "pipe match equations" $
        parseFails stmtP "match x { | 0 => return 1; }",
      testCase "old selective import ordering" $
        parseFails importP "import std.{address, uint256 as U256};",
      testCase "old namespace alias ordering" $
        parseFails importP "import std.dispatch as dispatch;",
      testCase "string-path import" $
        parseFails importP "import \"M/N.sol\";",
      testCase "hyphenated solcore pragma" $
        parseFails topDeclP "pragma no-coverage-condition;",
      testCase "equals type declaration" $
        parseFails topDeclP "type Word = word;",
      testCase "lambda arrow return" $
        parseFails expP "lam() -> word { return 0; }"
    ]

declarationShellTests :: TestTree
declarationShellTests =
  testGroup
    "Struct, interface, and library declarations"
    [ testCase "top-level struct retains field names and types" $
        parsesAs
          topDeclP
          "struct Pair<a> { left: a; right: word; }"
          (TDataDef (StructTy "Pair" [TyCon "a" []] ["left", "right"] [TyCon "a" [], word])),
      testCase "contract-local struct is a data declaration" $
        parsesAs
          topDeclP
          "contract C { struct Entry { key: word; value: bool; } }"
          ( TContr
              ( ContractShell
                  ContractKind
                  "C"
                  []
                  [CDataDecl (StructTy "Entry" [] ["key", "value"] [word, bool])]
              )
          ),
      testCase "interface contains body-less function signatures" $
        parsesAs
          topDeclP
          "interface Oracle { function read(key: word) external view returns (word); }"
          ( TContr
              ( ContractShell
                  InterfaceKind
                  "Oracle"
                  []
                  [ CSignatureDecl
                      True
                      ( SignatureWithSyntax
                          []
                          []
                          "read"
                          [Typed False "key" word]
                          (Just [ReturnItem False Nothing word])
                          [ VisibilityModifier VisibilityExternal,
                            MutabilityModifier MutabilityView
                          ]
                      )
                  ]
              )
          ),
      testCase "interface rejects a function body" $
        parseFails
          topDeclP
          "interface Oracle { function read() external returns (word) { return 0; } }",
      testCase "interface rejects state fields" $
        parseFails topDeclP "interface Oracle { value: word; }",
      testCase "library accepts contract-like fields, structs, and functions" $
        parsesAs
          topDeclP
          ( "library Math {"
              ++ " factor: word;"
              ++ " struct Result { value: word; }"
              ++ " function twice(x: word) internal pure returns (word) { return x + x; }"
              ++ " }"
          )
          ( TContr
              ( ContractShell
                  LibraryKind
                  "Math"
                  []
                  [ CFieldDecl (Field "factor" word Nothing),
                    CDataDecl (StructTy "Result" [] ["value"] [word]),
                    CFunDecl
                      ( FunDef
                          False
                          ( SignatureWithSyntax
                              []
                              []
                              "twice"
                              [Typed False "x" word]
                              (Just [ReturnItem False Nothing word])
                              [ VisibilityModifier VisibilityInternal,
                                MutabilityModifier MutabilityPure
                              ]
                          )
                          [Return (ExpPlus (var "x") (var "x"))]
                      )
                  ]
              )
          ),
      testCase "library rejects constructors" $
        parseFails
          topDeclP
          "library Math { constructor() { return; } }",
      testCase "name resolution rejects duplicate contract fields" $
        nameResolutionFails
          "contract C { value: word; value: bool; }",
      testCase "contract fields and functions retain separate namespaces" $
        nameResolutionSucceeds
          "contract C { value: word; function value() returns (word) { return 0; } }",
      testCase "contract fields and functions with distinct names do not collide" $
        nameResolutionSucceeds
          "contract C { value: word; function read() returns (word) { return value; } }",
      testCase "name resolution lowers a struct to a one-constructor data type" $
        case runParserE (sc *> topDeclP <* eof) "<test>" "struct Box { value: word; }" of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TDataDef
                        (Resolved.DataTy "Box" [] [Resolved.Constr (QualName "Box" "Box") [_]])
                      ]
                  ) ->
                    pure ()
              Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
              Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "name resolution preserves an interface signature without a body" $
        case runParserE (sc *> topDeclP <* eof) "<test>" "interface I { function f() external; }" of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TContr
                        (Resolved.Contract "I" [] [Resolved.CSignatureDecl True _])
                      ]
                  ) ->
                    pure ()
              Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
              Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "name resolution lowers source modifiers and named returns" $
        case
            runParserE
              (sc *> topDeclP <* eof)
              "<test>"
              "contract C { function pair() external payable returns (left: word, right: bool) { return (1, 0); } }"
          of
            Left err -> assertFailure ("Parse error:\n" ++ err)
            Right parsed -> do
              resolved <- nameResolution (CompUnit [] [parsed])
              case resolved of
                Right
                  ( Resolved.CompUnit
                      _
                      [ Resolved.TContr
                          (Resolved.Contract "C" [] [Resolved.CFunDecl (Resolved.FunDef isPublic sig _)])
                        ]
                    ) -> do
                      assertBool "external lowers to the semantic public bit" isPublic
                      assertBool "payable lowers to the semantic payable bit" (Resolved.sigPayable sig)
                      assertEqual
                        "return names are discarded only at semantic lowering"
                        ( Just
                            ( ResolvedTy.TyCon
                                "pair"
                                [ResolvedTy.TyCon "word" [], ResolvedTy.TyCon "bool" []]
                            )
                        )
                        (Resolved.sigReturn sig)
                Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
                Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "name resolution lowers a zero-arity function type explicitly" $
        case
            runParserE
              (sc *> topDeclP <* eof)
              "<test>"
              "type Callback is function() external returns (word);"
          of
            Left err -> assertFailure ("Parse error:\n" ++ err)
            Right parsed -> do
              resolved <- nameResolution (CompUnit [] [parsed])
              case resolved of
                Right
                  ( Resolved.CompUnit
                      _
                      [Resolved.TSym (Resolved.TySym "Callback" [] callbackTy)]
                    ) ->
                      assertEqual
                        "the existing semantic AST represents a nullary function by its result"
                        (ResolvedTy.TyCon "word" [])
                        callbackTy
                Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
                Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "new declaration shells survive source pretty-printing" $
        mapM_
          roundTripsTopDecl
          [ "struct Pair { x: word; y: bool; }",
            "interface Oracle { function read(key: word) external view returns (word); }",
            "library Math { function twice(x: word) internal pure returns (word) { return x + x; } }"
          ]
    ]
