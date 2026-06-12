{-# LANGUAGE OverloadedStrings #-}

module ParserTests (parserTests) where

import Common.LightYear (Parser, runParserE)
import Solcore.Frontend.Lexer.SolcoreLexer (sc)
import Solcore.Frontend.Parser.Decl (topDeclP)
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.Patterns (patP)
import Solcore.Frontend.Parser.SolcoreTypes (predP, typeP)
import Solcore.Frontend.Parser.Stmt (bodyP, stmtP)
import Solcore.Frontend.Syntax.Name (Name (..))
import Solcore.Frontend.Syntax.SyntaxTree
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
      declTests
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
      testCase "parameterized type" $
        parsesAs typeP "pair(word, bool)" (TyCon "pair" [word, bool]),
      testCase "two-parameter type" $
        parsesAs typeP "map(word, bool)" (TyCon "map" [word, bool]),
      testCase "arrow type" $
        parsesAs typeP "word -> bool" (TyCon "->" [word, bool]),
      testCase "arrow is right-associative" $
        parsesAs
          typeP
          "word -> bool -> word"
          (TyCon "->" [word, TyCon "->" [bool, word]]),
      testCase "unit type" $
        parsesAs typeP "()" (TyCon "()" []),
      testCase "parenthesized single type" $
        parsesAs typeP "(word)" word,
      testCase "pair type in parens" $
        parsesAs typeP "(word, bool)" (pairTy word bool),
      testCase "triple type in parens" $
        parsesAs typeP "(word, bool, word)" (pairTy word (pairTy bool word)),
      testCase "proxy type" $
        parsesAs typeP "@word" (TyCon "Proxy" [word]),
      testCase "qualified name in type" $
        parsesAs typeP "Foo.Bar" (TyCon (QualName "Foo" "Bar") []),
      testCase "arrow type in parens disambiguates" $
        parsesAs typeP "((word -> bool) -> word)" (TyCon "->" [TyCon "->" [word, bool], word]),
      -- Failure cases
      testCase "bare arrow fails" $
        parseFails typeP "->",
      testCase "unclosed paren fails" $
        parseFails typeP "(word"
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
        parsesAs predP "t:Functor(word)" (InCls "Functor" (TyCon "t" []) [word]),
      testCase "predicate with two params" $
        parsesAs predP "t:Bifunctor(word,bool)" (InCls "Bifunctor" (TyCon "t" []) [word, bool]),
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
      testCase "mul binds tighter than add" $
        parsesAs expP "1 + 2 * 3" (ExpPlus (lit 1) (ExpTimes (lit 2) (lit 3))),
      testCase "add then mul" $
        parsesAs expP "1 * 2 + 3" (ExpPlus (ExpTimes (lit 1) (lit 2)) (lit 3)),
      testCase "subtraction is left-associative" $
        parsesAs expP "3 - 2 - 1" (ExpMinus (ExpMinus (lit 3) (lit 2)) (lit 1)),
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
      testCase "if-then-else expression" $
        parsesAs expP "if x then 1 else 2" (ExpCond (var "x") (lit 1) (lit 2)),
      testCase "type annotation" $
        parsesAs expP "x : word" (TyExp (var "x") word),
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
      testCase "proxy expression" $
        parsesAs expP "@word" (ExpAt word),
      testCase "dot name without args" $
        parsesAs expP ".None" (ExpDotName "None" []),
      testCase "dot name with args" $
        parsesAs expP ".Some(1)" (ExpDotName "Some" [lit 1]),
      testCase "lambda no params" $
        parsesAs
          expP
          "lam() -> word { return 0; }"
          (Lam [] [Return (lit 0)] (Just word)),
      testCase "lambda with typed param" $
        parsesAs
          expP
          "lam(x:word) -> word { return x; }"
          (Lam [Typed False "x" word] [Return (var "x")] (Just word)),
      testCase "lambda without return type" $
        parsesAs
          expP
          "lam(x:word) { return x; }"
          (Lam [Typed False "x" word] [Return (var "x")] Nothing)
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
      testCase "return literal" $
        parsesAs stmtP "return 0;" (Return (lit 0)),
      testCase "return expression" $
        parsesAs stmtP "return x + 1;" (Return (ExpPlus (var "x") (lit 1))),
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
      testCase "call as statement no semicolon" $
        parsesAs stmtP "f()" (StmtExp (ExpName Nothing "f" [])),
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
      testCase "match one equation" $
        parsesAs
          stmtP
          "match x { | 0 => return 1; }"
          (Match [var "x"] [([PLit (IntLit 0)], [Return (lit 1)])]),
      testCase "match wildcard" $
        parsesAs
          stmtP
          "match x { | _ => return 0; }"
          (Match [var "x"] [([PWildcard], [Return (lit 0)])]),
      testCase "match constructor pattern" $
        parsesAs
          stmtP
          "match x { | Some(v) => return v; }"
          (Match [var "x"] [([Pat "Some" [Pat "v" []]], [Return (var "v")])]),
      testCase "match multiple equations" $
        parsesAs
          stmtP
          "match x { | 0 => return 0; | _ => return 1; }"
          ( Match
              [var "x"]
              [ ([PLit (IntLit 0)], [Return (lit 0)]),
                ([PWildcard], [Return (lit 1)])
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
          "function answer() -> word { return 42; }"
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
          "function id(x:word) -> word { return x; }"
          ( TFunDef
              ( FunDef
                  False
                  (Signature [] [] "id" [Typed False "x" word] False (Just word) False)
                  [Return (var "x")]
              )
          ),
      testCase "implicit return (single expr body)" $
        parsesAs
          topDeclP
          "function answer() -> word { 42 }"
          ( TFunDef
              ( FunDef
                  False
                  (Signature [] [] "answer" [] False (Just word) False)
                  [Return (lit 42)]
              )
          ),
      testCase "polymorphic function" $
        parsesAs
          topDeclP
          "forall a. function id(x:a) -> a { return x; }"
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
          "forall a. a:Eq => function eqSelf(x:a) -> bool { return x == x; }"
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
      testCase "empty data type" $
        parsesAs
          topDeclP
          "data Void;"
          (TDataDef (DataTy "Void" [] [])),
      testCase "data type with nullary constructors" $
        parsesAs
          topDeclP
          "data Bool = True | False;"
          (TDataDef (DataTy "Bool" [] [Constr "True" [], Constr "False" []])),
      testCase "data type with parameterized constructor" $
        parsesAs
          topDeclP
          "data Option(a) = Some(a) | None;"
          ( TDataDef
              ( DataTy
                  "Option"
                  [TyCon "a" []]
                  [Constr "Some" [TyCon "a" []], Constr "None" []]
              )
          ),
      testCase "type synonym no params" $
        parsesAs
          topDeclP
          "type Word = word;"
          (TSym (TySym "Word" [] word)),
      testCase "type synonym with params" $
        parsesAs
          topDeclP
          "type Pair(a, b) = (a, b);"
          ( TSym
              ( TySym
                  "Pair"
                  [TyCon "a" [], TyCon "b" []]
                  (pairTy (TyCon "a" []) (TyCon "b" []))
              )
          ),
      testCase "class with one method" $
        parsesAs
          topDeclP
          "forall a. class a:Eq { function eq(x:a, y:a) -> bool; }"
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
      testCase "class with context" $
        parsesAs
          topDeclP
          "forall a. a:Eq => class a:Ord { function cmp(x:a, y:a) -> word; }"
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
      testCase "instance with one method" $
        parsesAs
          topDeclP
          "instance word:Eq { function eq(x:word, y:word) -> bool { return x == y; } }"
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
      testCase "polymorphic instance" $
        parsesAs
          topDeclP
          "forall a. a:Eq => instance pair(a,a):Eq { function eq(x:pair(a,a), y:pair(a,a)) -> bool { return 0; } }"
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
          "contract C { function get() -> word { return x; } }"
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
          "contract C { public function get() -> word { return x; } }"
          ( TContr
              ( Contract
                  "C"
                  []
                  [ CFunDecl
                      ( FunDef
                          True
                          (Signature [] [] "get" [] False (Just word) False)
                          [Return (var "x")]
                      )
                  ]
              )
          ),
      -- `public` is only meaningful inside a contract; reject it elsewhere.
      testCase "top-level public function fails" $
        parseFails topDeclP "public function get() -> word { return 0; }",
      testCase "public instance method fails" $
        parseFails
          topDeclP
          "instance word:Eq { public function eq(x:word, y:word) -> bool { return x == y; } }"
    ]
