{-# LANGUAGE OverloadedStrings #-}

module ParserTests (parserTests) where

import Common.LightYear (Parser, runParserE)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Language.Yul (YLiteral (..), YulExp (..), YulStmt (..))
import Solcore.Desugarer.FieldAccess (fieldDesugarTopDecls)
import Solcore.Diagnostics (compilerErrorText)
import Solcore.Frontend.Lexer.SolcoreLexer (identifier, sc)
import Solcore.Frontend.Parser.Decl (importP, topDeclP)
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.Patterns (patP)
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnit)
import Solcore.Frontend.Parser.SolcoreTypes (predP, typeP)
import Solcore.Frontend.Parser.Stmt (bodyP, stmtP)
import Solcore.Frontend.Pretty.SolcorePretty qualified as SolcorePretty
import Solcore.Frontend.Pretty.TreePretty qualified as TreePretty
import Solcore.Frontend.Syntax.Contract qualified as Resolved
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Frontend.Syntax.Stmt qualified as ResolvedStmt
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

parseFailsContaining :: (Show a) => Parser a -> String -> String -> Assertion
parseFailsContaining p expected src =
  case runParserE (sc *> p <* eof) "<test>" src of
    Left err ->
      assertBool
        ("Expected parse error containing " ++ show expected ++ ", got:\n" ++ err)
        (expected `isInfixOf` err)
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

roundTripsExp :: String -> Assertion
roundTripsExp src =
  case runParserE (sc *> expP <* eof) "<test>" src of
    Left err -> assertFailure ("Initial parse error:\n" ++ err)
    Right parsed ->
      let rendered = TreePretty.pretty parsed
       in case runParserE (sc *> expP <* eof) "<pretty>" rendered of
            Left err ->
              assertFailure
                ( "Pretty-printed expression did not parse:\n"
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
    [ identifierTests,
      typeTests,
      predTests,
      patternTests,
      exprTests,
      stmtTests,
      compoundAssignmentResolutionTests,
      declTests,
      importTests,
      pragmaTests,
      declarationShellTests,
      keywordPrefixTests,
      legacySyntaxTests
    ]

compoundAssignmentResolutionTests :: TestTree
compoundAssignmentResolutionTests =
  testGroup
    "Compound assignment resolution"
    [ testCase "simple variable keeps direct assignment lowering" $ do
        body <-
          resolvedFunctionBody
            "update"
            "function update(value: word) { value += 1; }"
        case body of
          [ ResolvedStmt.AssignWithLocation
              _
              lhs@(ResolvedStmt.Var "value")
              (ResolvedStmt.Call Nothing (QualName "Add" "add") [readLhs, ResolvedStmt.Lit _])
            ] ->
              assertEqual "compound read uses the assigned variable" lhs readLhs
          other ->
            assertFailure ("Unexpected simple compound assignment lowering: " ++ show other),
      testCase "index expression and indexed base are each evaluated once" $ do
        body <-
          resolvedFunctionBody
            "update"
            ( unlines
                [ "function collection() returns (word) { return 0; }",
                  "function index() returns (word) { return 0; }",
                  "function update() { collection()[index()] += 1; }"
                ]
            )
        case body of
          [ ResolvedStmt.Block
              [ ResolvedStmt.Let False baseName Nothing (Just baseValue),
                ResolvedStmt.Let False indexName Nothing (Just indexValue),
                ResolvedStmt.AssignWithLocation
                  _
                  assignedLhs
                  (ResolvedStmt.Call Nothing (QualName "Add" "add") [readLhs, ResolvedStmt.Lit _])
                ]
            ] -> do
              assertEqual
                "base is evaluated once before the index"
                (ResolvedStmt.Call Nothing "collection" [])
                baseValue
              assertEqual
                "index is evaluated once"
                (ResolvedStmt.Call Nothing "index" [])
                indexValue
              let frozenLhs =
                    ResolvedStmt.Indexed
                      (ResolvedStmt.Var baseName)
                      (ResolvedStmt.Var indexName)
              assertEqual "write uses frozen address components" frozenLhs assignedLhs
              assertEqual "read uses the same frozen address components" frozenLhs readLhs
          other ->
            assertFailure ("Unexpected indexed compound assignment lowering: " ++ show other),
      testCase "member receiver is evaluated once" $ do
        body <-
          resolvedFunctionBody
            "update"
            ( unlines
                [ "function receiver() returns (word) { return 0; }",
                  "function update() { receiver().member += 1; }"
                ]
            )
        case body of
          [ ResolvedStmt.Block
              [ ResolvedStmt.Let False receiverName Nothing (Just receiverValue),
                ResolvedStmt.AssignWithLocation
                  _
                  assignedLhs
                  (ResolvedStmt.Call Nothing (QualName "Add" "add") [readLhs, ResolvedStmt.Lit _])
                ]
            ] -> do
              assertEqual
                "receiver is evaluated once"
                (ResolvedStmt.Call Nothing "receiver" [])
                receiverValue
              let frozenLhs =
                    ResolvedStmt.FieldAccess
                      (Just (ResolvedStmt.Var receiverName))
                      "member"
              assertEqual "write uses frozen receiver" frozenLhs assignedLhs
              assertEqual "read uses the same frozen receiver" frozenLhs readLhs
          other ->
            assertFailure ("Unexpected member compound assignment lowering: " ++ show other),
      testCase "field desugaring computes the indexed lvalue reference once" $ do
        body <-
          fieldDesugaredContractFunctionBody
            "Container"
            "update"
            ( unlines
                [ "contract Container {",
                  "  function index() returns (word) { return 0; }",
                  "  function update(collection: word) { collection[index()] += 1; }",
                  "}"
                ]
            )
        case body of
          [ ResolvedStmt.Block
              [ ResolvedStmt.Let False _ Nothing (Just indexValue),
                ResolvedStmt.Block
                  [ ResolvedStmt.Let False referenceName Nothing (Just referenceValue),
                    ResolvedStmt.StmtExp
                      ( ResolvedStmt.Call
                          Nothing
                          (QualName "Assign" "assign")
                          [ writeReference,
                            ResolvedStmt.Call
                              Nothing
                              (QualName "Add" "add")
                              [readReference, ResolvedStmt.Lit _]
                            ]
                        )
                    ]
                ]
            ] -> do
              assertEqual
                "index side effect is evaluated once"
                (ResolvedStmt.Call Nothing "index" [])
                indexValue
              case referenceValue of
                ResolvedStmt.Call
                  Nothing
                  "lidx"
                  [ResolvedStmt.Var "collection", ResolvedStmt.Var _] ->
                    pure ()
                other ->
                  assertFailure ("Expected one lidx address computation, got: " ++ show other)
              assertEqual
                "write uses the computed lvalue reference"
                (ResolvedStmt.Var referenceName)
                writeReference
              assertEqual
                "read loads through the same lvalue reference"
                ( ResolvedStmt.Call
                    Nothing
                    (QualName "CanStore" "load")
                    [ResolvedStmt.Var referenceName]
                )
                readReference
          other ->
            assertFailure ("Unexpected field-desugared compound assignment: " ++ show other),
      testCase "field desugaring computes a contract-field reference once" $ do
        body <-
          fieldDesugaredContractFunctionBody
            "Container"
            "update"
            ( unlines
                [ "contract Container {",
                  "  value: word;",
                  "  function update() { value += 1; }",
                  "}"
                ]
            )
        case body of
          [ ResolvedStmt.Block
              [ ResolvedStmt.Let False referenceName Nothing (Just referenceValue),
                ResolvedStmt.StmtExp
                  ( ResolvedStmt.Call
                      Nothing
                      (QualName "Assign" "assign")
                      [ writeReference,
                        ResolvedStmt.Call
                          Nothing
                          (QualName "Add" "add")
                          [readReference, ResolvedStmt.Lit _]
                        ]
                    )
                ]
            ] -> do
              case referenceValue of
                ResolvedStmt.Call Nothing (QualName "LVA" "acc") [_] ->
                  pure ()
                other ->
                  assertFailure ("Expected one LVA.acc address computation, got: " ++ show other)
              assertEqual
                "contract-field write uses the computed reference"
                (ResolvedStmt.Var referenceName)
                writeReference
              assertEqual
                "contract-field read loads through the same reference"
                ( ResolvedStmt.Call
                    Nothing
                    (QualName "CanStore" "load")
                    [ResolvedStmt.Var referenceName]
                )
                readReference
          other ->
            assertFailure ("Unexpected contract-field compound assignment: " ++ show other),
      testCase "all compound operators retain their semantic operation" $ do
        body <-
          resolvedFunctionBody
            "update"
            ( unlines
                [ "function update(value: word) {",
                  "  value += 1;",
                  "  value -= 1;",
                  "  value ^= 1;",
                  "  value &= 1;",
                  "  value |= 1;",
                  "  value %= 1;",
                  "}"
                ]
            )
        assertEqual
          "resolved compound operator targets"
          [ QualName "Add" "add",
            QualName "Sub" "sub",
            QualName "BitXor" "bxor",
            QualName "BitAnd" "band",
            QualName "BitOr" "bor",
            QualName "Mod" "mod"
          ]
          (map compoundOperator body)
    ]

resolvedFunctionBody :: Name -> String -> IO [ResolvedStmt.Stmt Name]
resolvedFunctionBody functionName source = do
  parsedResult <- parseCompUnit source
  parsed <-
    case parsedResult of
      Left err -> assertFailure ("Parse error:\n" ++ err)
      Right unit -> pure unit
  resolved <- nameResolution parsed
  case resolved of
    Left err -> assertFailure ("Name resolution failed: " ++ show err)
    Right (Resolved.CompUnit _ topDecls) ->
      case [ body
           | Resolved.TFunDef (Resolved.FunDef _ signature body) <- topDecls,
             Resolved.sigName signature == functionName
           ] of
        [body] -> pure body
        bodies ->
          assertFailure
            ( "Expected one resolved function body for "
                ++ show functionName
                ++ ", got "
                ++ show (length bodies)
            )

compoundOperator :: ResolvedStmt.Stmt Name -> Name
compoundOperator
  ( ResolvedStmt.AssignWithLocation
      _
      _
      (ResolvedStmt.Call Nothing operator [_, _])
    ) = operator
compoundOperator stmt =
  error ("Unexpected simple compound assignment lowering: " ++ show stmt)

fieldDesugaredContractFunctionBody ::
  Name ->
  Name ->
  String ->
  IO [ResolvedStmt.Stmt Name]
fieldDesugaredContractFunctionBody contractName functionName source = do
  parsedResult <- parseCompUnit source
  parsed <-
    case parsedResult of
      Left err -> assertFailure ("Parse error:\n" ++ err)
      Right unit -> pure unit
  resolved <- nameResolution parsed
  case resolved of
    Left err -> assertFailure ("Name resolution failed: " ++ show err)
    Right (Resolved.CompUnit _ topDecls) ->
      case [ body
           | Resolved.TContr contract <- fieldDesugarTopDecls topDecls,
             Resolved.name contract == contractName,
             Resolved.CFunDecl (Resolved.FunDef _ signature body) <- Resolved.decls contract,
             Resolved.sigName signature == functionName
           ] of
        [body] -> pure body
        bodies ->
          assertFailure
            ( "Expected one field-desugared function body for "
                ++ show functionName
                ++ ", got "
                ++ show (length bodies)
            )

word :: Ty
word = TyCon "word" []

bool :: Ty
bool = TyCon "bool" []

identifierTests :: TestTree
identifierTests =
  testGroup
    "Identifiers"
    [ testCase "leading underscore" $
        parsesAs identifier "_id" "_id",
      testCase "multiple leading underscores and digits" $
        parsesAs identifier "__value2" "__value2",
      testCase "underscore-prefixed expression name" $
        parsesAs expP "_value" (var "_value"),
      testCase "boolean literals cannot be rebound as identifiers" $ do
        parseFails identifier "true"
        parseFails identifier "false"
        parseFails
          topDeclP
          "function invalid(true: word, false: word) returns (word) { return true; }"
    ]

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
      testCase "type suffixes may interleave arrays and locations" $
        parsesAs
          typeP
          "word[] memory[] storage"
          ( TyCon
              "storage"
              [TyCon "array" [TyCon "memory" [TyCon "array" [word]]]]
          ),
      testCase "repeated data locations remain distinct type wrappers" $
        parsesAs
          typeP
          "word memory storage"
          (TyCon "storage" [TyCon "memory" [word]]),
      testCase "interleaved type suffixes survive source pretty-printing" $
        roundTripsType "word[] memory[] storage",
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
      testCase "underscore-prefixed name is not a wildcard" $
        parsesAs patP "_value" (Pat "_value" []),
      testCase "wildcard cannot take constructor arguments" $
        parseFails patP "_(value)",
      testCase "integer literal" $
        parsesAs patP "42" (PLit (IntLit 42)),
      testCase "string literal" $
        parsesAs patP "\"hi\"" (PLit (StrLit "hi")),
      testCase "boolean literal patterns" $ do
        parsesAs patP "true" (Pat "true" [])
        parsesAs patP "false" (Pat "false" []),
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
      testCase "dot boolean pattern" $
        parsesAs patP ".true" (PatDot "true" []),
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
      testCase "string literal carriage-return escape" $
        parsesAs expP "\"line\\rbreak\"" (Lit (StrLit "line\rbreak")),
      testCase "boolean literals" $ do
        parsesAs expP "true" (var "true")
        parsesAs expP "false" (var "false"),
      testCase "variable" $
        parsesAs expP "x" (var "x"),
      testCase "nullary call" $
        parsesAs expP "f()" (ExpName Nothing "f" []),
      testCase "unary call" $
        parsesAs expP "f(1)" (ExpName Nothing "f" [lit 1]),
      testCase "binary call" $
        parsesAs expP "f(1, 2)" (ExpName Nothing "f" [lit 1, lit 2]),
      testCase "parenthesized name call keeps the direct-call source shape" $
        parsesAs expP "(f)(1)" (ExpName Nothing "f" [lit 1]),
      testCase "call result can be called again" $
        parsesAs
          expP
          "f(1)(2)"
          (ExpApply (ExpName Nothing "f" [lit 1]) [lit 2]),
      testCase "postfix call supports zero and multiple arguments" $ do
        parsesAs
          expP
          "f()()"
          (ExpApply (ExpName Nothing "f" []) [])
        parsesAs
          expP
          "f(1)(2, 3, 4)"
          (ExpApply (ExpName Nothing "f" [lit 1]) [lit 2, lit 3, lit 4]),
      testCase "indexed expression can be called" $
        parsesAs
          expP
          "callbacks[i](x)"
          (ExpApply (ExpIndexed (var "callbacks") (var "i")) [var "x"]),
      testCase "conditional expression can be called" $
        parsesAs
          expP
          "(condition ? f : g)(x)"
          (ExpApply (ExpCond (var "condition") (var "f") (var "g")) [var "x"]),
      testCase "lambda can be called immediately" $
        parsesAs
          expP
          "(lam(x: word) returns (word) { return x; })(1)"
          (ExpApply (Lam [Typed False "x" word] [Return (var "x")] (Just word)) [lit 1]),
      testCase "arbitrary postfix calls survive source pretty-printing" $
        mapM_
          roundTripsExp
          [ "f(1)(2)",
            "callbacks[i](x)",
            "(condition ? f : g)(x)",
            "(lam(x: word) returns (word) { return x; })(1)"
          ],
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
      testCase "relational operators are non-associative" $
        parseFails expP "a < b < c",
      testCase "equality operators are non-associative" $
        parseFails expP "a == b != c",
      testCase "arith tighter than comparison" $
        parsesAs
          expP
          "a + b == c + d"
          (ExpEE (ExpPlus (var "a") (var "b")) (ExpPlus (var "c") (var "d"))),
      testCase "bitwise and binds tighter than comparison" $
        parsesAs
          expP
          "a & b < c"
          (ExpLT (ExpBAnd (var "a") (var "b")) (var "c")),
      testCase "comparison binds tighter than equality" $
        parsesAs
          expP
          "a < b == c"
          (ExpEE (ExpLT (var "a") (var "b")) (var "c")),
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
      testCase "conversion target does not consume relational or shift operators" $ do
        parsesAs
          expP
          "x as word < y"
          (ExpLT (TyExp (var "x") word) (var "y"))
        parsesAs
          expP
          "x as word <= y"
          (ExpLE (TyExp (var "x") word) (var "y"))
        parsesAs
          expP
          "x as word << y"
          (ExpShiftL (TyExp (var "x") word) (var "y")),
      testCase "converted comparisons and shifts survive source pretty-printing" $
        mapM_
          roundTripsExp
          [ "x as word < y",
            "x as word <= y",
            "x as word << y"
          ],
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
      testCase "dot boolean name" $
        parsesAs expP ".true" (ExpDotName "true" []),
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
          (Lam [Typed False "x" word] [Return (var "x")] Nothing),
      testCase "name resolution lowers arbitrary calls and packs their arguments" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          ( "function packing(x: word, y: word, z: word) {"
              ++ " (lam() returns (word) { return 1; })();"
              ++ " (lam(a: word) returns (word) { return a; })(x);"
              ++ " (lam(a: word, b: word, c: word) returns (word) { return a; })(x, y, z);"
              ++ " }"
          ) of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TFunDef
                        ( Resolved.FunDef
                            _
                            _
                            [ ResolvedStmt.StmtExp
                                (ResolvedStmt.Call Nothing invoke0 [ResolvedStmt.Lam _ _ _, packed0]),
                              ResolvedStmt.StmtExp
                                (ResolvedStmt.Call Nothing invoke1 [ResolvedStmt.Lam _ _ _, packed1]),
                              ResolvedStmt.StmtExp
                                (ResolvedStmt.Call Nothing invokeMany [ResolvedStmt.Lam _ _ _, packedMany])
                              ]
                          )
                      ]
                  ) -> do
                  let expectedInvoke = QualName "invokable" "invoke"
                  assertEqual "zero-argument invoke target" expectedInvoke invoke0
                  assertEqual "single-argument invoke target" expectedInvoke invoke1
                  assertEqual "multi-argument invoke target" expectedInvoke invokeMany
                  case packed0 of
                    ResolvedStmt.Con "()" [] -> pure ()
                    other -> assertFailure ("Unexpected zero-argument packing: " ++ show other)
                  case packed1 of
                    ResolvedStmt.Var "x" -> pure ()
                    other -> assertFailure ("Unexpected single-argument packing: " ++ show other)
                  case packedMany of
                    ResolvedStmt.Con
                      "pair"
                      [ ResolvedStmt.Var "x",
                        ResolvedStmt.Con
                          "pair"
                          [ResolvedStmt.Var "y", ResolvedStmt.Var "z"]
                        ] ->
                        pure ()
                    other -> assertFailure ("Unexpected multi-argument packing: " ++ show other)
              Right got -> assertFailure ("Unexpected name-resolution shape: " ++ show got)
              Left err -> assertFailure ("Name resolution failed: " ++ show err)
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
      testCase "tuple destructuring distinguishes a leading-underscore binder from a wildcard" $
        parsesAs
          stmtP
          "let (_value, _) = readResult();"
          ( LetPattern
              False
              (Pat "pair" [Pat "_value" [], PWildcard])
              Nothing
              (ExpName Nothing "readResult" [])
          ),
      testCase "tuple destructuring rejects duplicate leading-underscore binders" $
        parseFails stmtP "let (_value, _value) = readResult();",
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
      testCase "bare return remains distinct from an explicit unit return" $
        parsesAs stmtP "return;" BareReturn,
      testCase "explicit unit return remains an expression return" $
        parsesAs stmtP "return ();" (Return unitExp),
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
      testCase "for initializer accepts tuple destructuring let" $
        parsesAs
          stmtP
          "for (let (left, right): (word, bool) = readResult(); keepGoing; ) { }"
          ( For
              ( LetPattern
                  False
                  (Pat "pair" [Pat "left" [], Pat "right" []])
                  (Just (TyCon "pair" [word, bool]))
                  (ExpName Nothing "readResult" [])
              )
              (var "keepGoing")
              EmptyStmt
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
      testCase "bare revert remains distinct in the source AST" $
        parsesAs
          stmtP
          "revert;"
          Revert,
      testCase "revert is reserved for the statement form" $ do
        parseFails identifier "revert"
        parseFails patP "revert"
        parsesAs stmtP "revert;" Revert,
      testCase "bare revert survives source pretty-printing" $
        roundTripsStmt "revert;",
      testCase "Yul control-flow keywords remain statements" $
        parsesAs
          stmtP
          "assembly { break continue leave }"
          (Asm [YBreak, YContinue, YLeave]),
      testCase "Yul function declarations preserve arguments and returns" $
        parsesAs
          stmtP
          "assembly { function pair(x, y) -> left, right { left, right := pair(x, y) } }"
          ( Asm
              [ YFun
                  "pair"
                  ["x", "y"]
                  (Just ["left", "right"])
                  [ YAssign
                      ["left", "right"]
                      (YCall "pair" [YIdent "x", YIdent "y"])
                  ]
              ]
          ),
      testCase "Yul control flow accepts booleans and dollar identifiers" $
        parsesAs
          stmtP
          ( "assembly {"
              ++ " let $flag := true"
              ++ " if $flag { continue }"
              ++ " for {} false {} { break }"
              ++ " switch $flag case true { leave } default {}"
              ++ " }"
          )
          ( Asm
              [ YLet ["$flag"] (Just (YLit YulTrue)),
                YIf (YIdent "$flag") [YContinue],
                YFor [] (YLit YulFalse) [] [YBreak],
                YSwitch
                  (YIdent "$flag")
                  [(YulTrue, [YLeave])]
                  (Just [])
              ]
          ),
      testCase "Yul keyword prefixes remain ordinary identifiers" $
        parsesAs
          stmtP
          "assembly { let x := trueValue breakFoo() functionFoo() }"
          ( Asm
              [ YLet ["x"] (Just (YIdent "trueValue")),
                YExp (YCall "breakFoo" []),
                YExp (YCall "functionFoo" [])
              ]
          ),
      testCase "Yul metadata expressions accept backtick and interpolation spellings" $ do
        parsesAs
          stmtP
          "assembly { let first := `backtickHole` let second := ${interpolationHole} }"
          ( Asm
              [ YLet ["first"] (Just (YMeta "backtickHole")),
                YLet ["second"] (Just (YMeta "interpolationHole"))
              ]
          ),
      testCase "Yul metadata expressions survive source pretty-printing" $
        mapM_
          roundTripsStmt
          [ "assembly { let x := `hole` }",
            "assembly { let x := ${hole} }",
            "assembly { let x := `a}b` }",
            "assembly { let x := ${a`b} }"
          ],
      testCase "Yul string literals survive source pretty-printing" $
        mapM_
          roundTripsStmt
          [ "assembly { let x := \"a\\\"b\" }",
            "assembly { let x := \"a\\\\b\" }",
            "assembly { let x := \"a\\nb\" }",
            "assembly { let x := \"a\\tb\" }",
            "assembly { let x := \"a\\rb\" }"
          ],
      testCase "Yul let requires at least one name" $
        parseFails
          stmtP
          "assembly { let := 1 }",
      testCase "Yul assignment requires at least one name" $
        parseFails
          stmtP
          "assembly { := 1 }",
      testCase "Yul function return arrow requires at least one name" $
        parseFails
          stmtP
          "assembly { function invalid() -> {} }",
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
                  [BareReturn]
              )
          ),
      testCase "underscore-prefixed function and parameter names parse" $
        parsesAs
          topDeclP
          "function _id(_value: word) returns (word) { return _value; }"
          ( TFunDef
              ( FunDef
                  False
                  (Signature [] [] "_id" [Typed False "_value" word] False (Just word) False)
                  [Return (var "_value")]
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
      testCase "duplicate top-level struct fields fail name resolution" $
        nameResolutionFails
          "struct Pair { value: word; value: bool; }",
      testCase "duplicate nested struct fields fail name resolution" $
        nameResolutionFails
          "contract C { struct Pair { value: word; value: bool; } }",
      testCase "transparent type alias" $
        parsesAs
          topDeclP
          "alias Word = word;"
          (TSym (TySym "Word" [] word)),
      testCase "generic transparent type alias" $
        parsesAs
          topDeclP
          "alias Pair<a, b> = (a, b);"
          ( TSym
              ( TySym
                  "Pair"
                  [TyCon "a" [], TyCon "b" []]
                  (pairTy (TyCon "a" []) (TyCon "b" []))
              )
          ),
      testCase "alias is reserved as a declaration keyword" $
        parseFails identifier "alias",
      testCase "nominal type syntax is not treated as a transparent alias" $
        parseFailsContaining
          topDeclP
          "user-defined value types declared with `type ... is ...` are not yet implemented"
          "type Word is word;",
      testCase "transparent aliases survive source pretty-printing" $
        roundTripsTopDecl "alias Pair<a, b> = (a, b);",
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
      testCase "trait declarations cannot introduce qualified names" $
        parseFails
          topDeclP
          "trait Imported.Eq<a> { function eq(x:a, y:a) returns (bool); }",
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
                          [BareReturn]
                      ),
                    CFunDecl
                      ( FunDef
                          False
                          (SignatureWithSyntax [] [] "viewFn" [] Nothing [MutabilityModifier MutabilityView])
                          [BareReturn]
                      ),
                    CFunDecl
                      ( FunDef
                          False
                          (SignatureWithSyntax [] [] "privateFn" [] Nothing [VisibilityModifier VisibilityPrivate])
                          [BareReturn]
                      ),
                    CFunDecl
                      ( FunDef
                          False
                          (SignatureWithSyntax [] [] "internalFn" [] Nothing [VisibilityModifier VisibilityInternal])
                          [BareReturn]
                      ),
                    CFunDecl
                      ( FunDef
                          True
                          (SignatureWithSyntax [] [] "externalFn" [] Nothing [VisibilityModifier VisibilityExternal])
                          [BareReturn]
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
                  [CConstrDecl (Constructor [Typed False "x" word] [BareReturn] False)]
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
                  [CConstrDecl (Constructor [Typed False "x" word] [BareReturn] True)]
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
                          [BareReturn]
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
      testCase "module functions retain pure and view without contract visibility" $ do
        parsesAs
          topDeclP
          "function pureFn() pure { return; }"
          ( TFunDef
              ( FunDef
                  False
                  (SignatureWithSyntax [] [] "pureFn" [] Nothing [MutabilityModifier MutabilityPure])
                  [BareReturn]
              )
          )
        parsesAs
          topDeclP
          "function viewFn() view { return; }"
          ( TFunDef
              ( FunDef
                  False
                  (SignatureWithSyntax [] [] "viewFn" [] Nothing [MutabilityModifier MutabilityView])
                  [BareReturn]
              )
          ),
      testCase "module functions reject contract visibility and payable" $
        mapM_
          (parseFails topDeclP)
          [ "function publicFn() public { return; }",
            "function externalFn() external { return; }",
            "function internalFn() internal { return; }",
            "function privateFn() private { return; }",
            "function payableFn() payable { return; }"
          ],
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
          ),
      testCase "selective import requires at least one item" $
        parseFails importP "import {} from std;",
      testCase "hiding clause requires at least one item" $
        parseFails importP "import {foo} from std hiding {};"
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
          (TPragmaDecl (Pragma NoGenericInstanceFor (DisableFor ("MyType" :| [])))),
      testCase "disable generic instance generation for a nested type" $ do
        let source =
              "pragma solcore noGenericInstanceFor Capsule.Token;"
        parsesAs
          topDeclP
          source
          ( TPragmaDecl
              ( Pragma
                  NoGenericInstanceFor
                  (DisableFor (QualName "Capsule" "Token" :| []))
              )
          )
        roundTripsTopDecl source
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
      testCase "underscore-prefixed struct and field names parse" $
        parsesAs
          topDeclP
          "struct _Record { _value: word; }"
          (TDataDef (StructTy "_Record" [] ["_value"] [word])),
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
      testCase "interface rejects omitted function visibility" $
        parseFailsContaining
          topDeclP
          "exactly one `external`"
          "interface Oracle { function read() returns (word); }",
      testCase "interface rejects public function visibility" $
        parseFailsContaining
          topDeclP
          "exactly one `external`"
          "interface Oracle { function read() public returns (word); }",
      testCase "interface rejects private function visibility" $
        parseFailsContaining
          topDeclP
          "exactly one `external`"
          "interface Oracle { function read() private returns (word); }",
      testCase "interface rejects internal function visibility" $
        parseFailsContaining
          topDeclP
          "exactly one `external`"
          "interface Oracle { function read() internal returns (word); }",
      testCase "interface accepts external pure and payable signatures" $ do
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          ( "interface Oracle {"
              ++ " function compute() external pure returns (word);"
              ++ " function deposit() external payable;"
              ++ " }"
          ) of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right
            ( TContr
                ( ContractShell
                    InterfaceKind
                    _
                    _
                    [ CSignatureDecl
                        True
                        (SignatureWithSyntax _ _ _ _ _ computeModifiers),
                      CSignatureDecl
                        True
                        (SignatureWithSyntax _ _ _ _ _ depositModifiers)
                      ]
                  )
              ) -> do
              assertEqual
                "pure interface signature modifiers"
                [ VisibilityModifier VisibilityExternal,
                  MutabilityModifier MutabilityPure
                ]
                computeModifiers
              assertEqual
                "payable interface signature modifiers"
                [ VisibilityModifier VisibilityExternal,
                  MutabilityModifier MutabilityPayable
                ]
                depositModifiers
          Right got -> assertFailure ("Unexpected interface shape: " ++ show got),
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
      testCase "underscore-prefixed function and parameter names resolve" $
        nameResolutionSucceeds
          "function _id(_value: word) returns (word) { return _value; }",
      testCase "underscore-prefixed struct and field names resolve" $
        nameResolutionSucceeds
          "struct _Record { _value: word; }",
      testCase "underscore-prefixed match binders resolve" $
        nameResolutionSucceeds
          ( "function select(_input: word) returns (word) {"
              ++ " match (_input) { case _value { return _value; } }"
              ++ " return 0;"
              ++ " }"
          ),
      testCase "wildcard patterns do not bind the standalone underscore" $
        nameResolutionFails
          ( "function select(_input: word) returns (word) {"
              ++ " match (_input) { case _ { return _; } }"
              ++ " return 0;"
              ++ " }"
          ),
      testCase "name resolution preserves struct metadata and semantic pretty syntax" $
        case runParserE (sc *> topDeclP <* eof) "<test>" "struct Box { value: word; }" of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TDataDef
                        dt@( Resolved.DataTyWithKind
                               (Resolved.StructKind ["value"])
                               "Box"
                               []
                               [Resolved.Constr (QualName "Box" "Box") [ResolvedTy.TyCon "word" []]]
                             )
                      ]
                  ) -> do
                  let rendered = SolcorePretty.pretty dt
                  assertBool
                    ("semantic struct pretty output lost its declaration kind:\n" ++ rendered)
                    ("struct Box" `isInfixOf` rendered)
                  assertBool
                    ("semantic struct pretty output lost its named field:\n" ++ rendered)
                    ("value: word;" `isInfixOf` rendered)
              Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
              Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "value member reads retain their receiver with and without name collisions" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          ( "contract C {"
              ++ " struct Pair { x: word; }"
              ++ " function collision(p: Pair, x: word) returns (word) { return p.x; }"
              ++ " function noCollision(p: Pair) returns (word) { return p.x; }"
              ++ " }"
          ) of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TContr
                        ( Resolved.ContractWithKind
                            _
                            _
                            _
                            [ _,
                              Resolved.CFunDecl
                                ( Resolved.FunDef
                                    _
                                    _
                                    [ResolvedStmt.Return (ResolvedStmt.FieldAccess (Just (ResolvedStmt.Var "p")) "x")]
                                  ),
                              Resolved.CFunDecl
                                ( Resolved.FunDef
                                    _
                                    _
                                    [ResolvedStmt.Return (ResolvedStmt.FieldAccess (Just (ResolvedStmt.Var "p")) "x")]
                                  )
                              ]
                          )
                      ]
                  ) ->
                  pure ()
              Right got ->
                assertFailure
                  ("value receiver was dropped during name resolution: " ++ show got)
              Left err ->
                assertFailure ("Name resolution failed: " ++ show err),
      testCase "type qualifiers still win over same-named parameters" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          ( "contract C {"
              ++ " enum Choice { Left }"
              ++ " function pick(Left: word) returns (Choice) { return Choice.Left; }"
              ++ " }"
          ) of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TContr
                        ( Resolved.ContractWithKind
                            _
                            _
                            _
                            [ _,
                              Resolved.CFunDecl
                                ( Resolved.FunDef
                                    _
                                    _
                                    [ ResolvedStmt.Return
                                        ( ResolvedStmt.Con
                                            (QualName (QualName "C" "Choice") "Left")
                                            []
                                          )
                                      ]
                                  )
                              ]
                          )
                      ]
                  ) ->
                  pure ()
              Right got ->
                assertFailure
                  ("type qualifier was captured by a parameter: " ++ show got)
              Left err ->
                assertFailure ("Name resolution failed: " ++ show err),
      testCase "contract names qualify contract-local constructors" $
        nameResolutionSucceeds
          ( "contract C {"
              ++ " struct S { value: word; }"
              ++ " function make() returns (S) { return C.S.S(1); }"
              ++ " }"
          ),
      testCase "resolved pretty-printing preserves one named tuple return item" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          ( "function pairResult() returns (result: (word, bool)) {"
              ++ " result = (1, true);"
              ++ " return;"
              ++ " }"
          ) of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Left err ->
                assertFailure ("Name resolution failed: " ++ show err)
              Right (Resolved.CompUnit _ [resolvedDecl]) -> do
                let rendered = SolcorePretty.pretty resolvedDecl
                case runParserE (sc *> topDeclP <* eof) "<pretty>" rendered of
                  Left err ->
                    assertFailure
                      ( "Resolved pretty output did not parse:\n"
                          ++ rendered
                          ++ "\n"
                          ++ err
                      )
                  Right reparsed -> do
                    reresolved <- nameResolution (CompUnit [] [reparsed])
                    case reresolved of
                      Right
                        ( Resolved.CompUnit
                            _
                            [ Resolved.TFunDef
                                (Resolved.FunDef _ signature _)
                              ]
                          ) ->
                          assertEqual
                            "return name and tuple boundary survive semantic pretty-printing"
                            [ Resolved.SignatureReturnItem
                                False
                                (Just "result")
                                ( ResolvedTy.TyCon
                                    "pair"
                                    [ ResolvedTy.TyCon "word" [],
                                      ResolvedTy.TyCon "bool" []
                                    ]
                                )
                            ]
                            (Resolved.sigReturnItems signature)
                      Right got ->
                        assertFailure
                          ("Unexpected re-resolved output: " ++ show got)
                      Left err ->
                        assertFailure
                          ( "Resolved pretty output failed name resolution:\n"
                              ++ rendered
                              ++ "\n"
                              ++ show err
                          )
              Right got ->
                assertFailure ("Unexpected resolved output: " ++ show got),
      testCase "resolved pretty-printing does not invent a unit return item" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          "contract C { function nop() public { return; } }" of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Left err ->
                assertFailure ("Name resolution failed: " ++ show err)
              Right (Resolved.CompUnit _ [resolvedDecl]) -> do
                let rendered = SolcorePretty.pretty resolvedDecl
                assertBool
                  ("semantic pretty output invented a return clause:\n" ++ rendered)
                  (not ("returns" `isInfixOf` rendered))
                nameResolutionSucceeds rendered
              Right got ->
                assertFailure ("Unexpected resolved output: " ++ show got),
      testCase "resolved pretty-printing preserves an explicit unit return item" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          "function unitValue() returns (()) { return (); }" of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Left err ->
                assertFailure ("Name resolution failed: " ++ show err)
              Right (Resolved.CompUnit _ [resolvedDecl]) -> do
                let rendered = SolcorePretty.pretty resolvedDecl
                assertBool
                  ("semantic pretty output lost the explicit unit item:\n" ++ rendered)
                  ("returns (())" `isInfixOf` rendered)
                nameResolutionSucceeds rendered
              Right got ->
                assertFailure ("Unexpected resolved output: " ++ show got),
      testCase "resolved pretty-printing keeps bare contract fields reusable" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          ( "contract C {"
              ++ " value: word;"
              ++ " function read() returns (word) { return value; }"
              ++ " }"
          ) of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Left err ->
                assertFailure ("Name resolution failed: " ++ show err)
              Right (Resolved.CompUnit _ [resolvedDecl]) -> do
                let rendered = SolcorePretty.pretty resolvedDecl
                assertBool
                  ("semantic pretty output invented an undefined receiver:\n" ++ rendered)
                  (not ("this." `isInfixOf` rendered))
                case runParserE (sc *> topDeclP <* eof) "<pretty>" rendered of
                  Left err ->
                    assertFailure
                      ( "Resolved pretty output did not parse:\n"
                          ++ rendered
                          ++ "\n"
                          ++ err
                      )
                  Right reparsed -> do
                    reresolved <- nameResolution (CompUnit [] [reparsed])
                    case reresolved of
                      Left err ->
                        assertFailure
                          ( "Resolved pretty output failed name resolution:\n"
                              ++ rendered
                              ++ "\n"
                              ++ show err
                          )
                      Right _ -> pure ()
              Right got ->
                assertFailure ("Unexpected resolved output: " ++ show got),
      testCase "value member calls fail explicitly during name resolution" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          ( "contract C {"
              ++ " struct Pair { x: word; }"
              ++ " function bad(p: Pair) returns (word) { return p.x(); }"
              ++ " }"
          ) of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Left err ->
                assertBool
                  ("expected SC0124, got:\n" ++ compilerErrorText err)
                  ("SC0124" `isInfixOf` compilerErrorText err)
              Right got ->
                assertFailure
                  ("value member call was silently lowered: " ++ show got),
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
                        ( Resolved.ContractWithKind
                            Resolved.InterfaceKind
                            "I"
                            []
                            [Resolved.CSignatureDecl True signature]
                          )
                      ]
                  ) ->
                  assertEqual
                    "exact interface modifiers survive name resolution"
                    [ Resolved.VisibilityModifier Resolved.VisibilityExternal
                    ]
                    (Resolved.sigModifiers signature)
              Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
              Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "name resolution preserves a library declaration kind" $
        case runParserE (sc *> topDeclP <* eof) "<test>" "library L { function f() internal { return; } }" of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TContr
                        ( Resolved.ContractWithKind
                            Resolved.LibraryKind
                            "L"
                            []
                            [Resolved.CFunDecl _]
                          )
                      ]
                  ) ->
                  pure ()
              Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
              Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "name resolution lowers source modifiers and named returns" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          "contract C { function pair() external payable returns (left: word, right: bool) { return (1, 0); } }" of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TContr
                        ( Resolved.ContractWithKind
                            Resolved.ContractKind
                            "C"
                            []
                            [Resolved.CFunDecl (Resolved.FunDef isPublic sig _)]
                          )
                      ]
                  ) -> do
                  assertBool "external lowers to the semantic public bit" isPublic
                  assertBool "payable lowers to the semantic payable bit" (Resolved.sigPayable sig)
                  assertEqual
                    "return items still aggregate to the backend result type"
                    ( Just
                        ( ResolvedTy.TyCon
                            "pair"
                            [ResolvedTy.TyCon "word" [], ResolvedTy.TyCon "bool" []]
                        )
                    )
                    (Resolved.sigReturn sig)
                  assertEqual
                    "semantic lowering preserves return-item names and comptime modes"
                    [(Just "left", False), (Just "right", False)]
                    [ ( Resolved.signatureReturnItemName returnItem,
                        Resolved.signatureReturnItemComptime returnItem
                      )
                    | returnItem <- Resolved.sigReturnItems sig
                    ]
              Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
              Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "name resolution deliberately lowers supported internal function types" $
        case runParserE
          (sc *> topDeclP <* eof)
          "<test>"
          "function apply(f: function(word) internal returns (word, bool), x: word) returns (word, bool) { return f(x); }" of
          Left err -> assertFailure ("Parse error:\n" ++ err)
          Right parsed -> do
            resolved <- nameResolution (CompUnit [] [parsed])
            case resolved of
              Right
                ( Resolved.CompUnit
                    _
                    [ Resolved.TFunDef
                        ( Resolved.FunDef
                            _
                            signature
                            _
                          )
                      ]
                  ) -> do
                  callbackTy <-
                    case Resolved.sigParams signature of
                      ResolvedStmt.Typed _ "f" ty : _ -> pure ty
                      params ->
                        assertFailure ("Unexpected resolved parameters: " ++ show params)
                  assertEqual
                    "supported internal function types lower to the existing arrow representation"
                    ( ResolvedTy.funtype
                        [ResolvedTy.TyCon "word" []]
                        ( ResolvedTy.TyCon
                            "pair"
                            [ ResolvedTy.TyCon "word" [],
                              ResolvedTy.TyCon "bool" []
                            ]
                        )
                    )
                    callbackTy
              Right got -> assertFailure ("Unexpected lowering result: " ++ show got)
              Left err -> assertFailure ("Name resolution failed: " ++ show err),
      testCase "name resolution rejects external function types instead of treating them as internal" $
        assertFunctionTypeResolutionError
          "external function types are not supported"
          "function bad(f: function(word) external returns (word)) returns (word) { return 0; }",
      testCase "name resolution rejects nullary function types instead of collapsing them to the result" $
        assertFunctionTypeResolutionError
          "zero-parameter function types are not supported"
          "function bad(f: function() internal returns (word)) returns (word) { return 0; }",
      testCase "new declaration shells survive source pretty-printing" $
        mapM_
          roundTripsTopDecl
          [ "struct Pair { x: word; y: bool; }",
            "struct _Record { _value: word; }",
            "function _id(_value: word) returns (word) { return _value; }",
            "interface Oracle { function read(key: word) external view returns (word); }",
            "library Math { function twice(x: word) internal pure returns (word) { return x + x; } }"
          ]
    ]

assertFunctionTypeResolutionError :: String -> String -> Assertion
assertFunctionTypeResolutionError expectedMessage source =
  case runParserE (sc *> topDeclP <* eof) "<test>" source of
    Left err -> assertFailure ("Parse error:\n" ++ err)
    Right parsed -> do
      resolved <- nameResolution (CompUnit [] [parsed])
      case resolved of
        Left err -> do
          let rendered = compilerErrorText err
          assertBool
            ("Expected SC0122 diagnostic, got:\n" ++ rendered)
            ("SC0122" `isInfixOf` rendered)
          assertBool
            ("Expected diagnostic message " ++ show expectedMessage ++ ", got:\n" ++ rendered)
            (expectedMessage `isInfixOf` rendered)
        Right got ->
          assertFailure
            ("Expected name-resolution failure but resolved: " ++ show got)
