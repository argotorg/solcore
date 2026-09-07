{-# LANGUAGE OverloadedStrings #-}

module NewSyntaxTypeTests (newSyntaxTypeTests) where

import Common.LightYear (Parser, eof, runParserE)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.SolcoreTypes
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree
import Test.Tasty
import Test.Tasty.HUnit

newSyntaxTypeTests :: TestTree
newSyntaxTypeTests =
  testGroup
    "new syntax types and lexer"
    [ testCase "nested proxy, mapping and function types" $
        parsesAs
          typeP
          "function(@mapping(word => option<bool,>),) returns (comptime<word>,)"
          ( FunctionTy
              [TyCon "Proxy" [TyCon "mapping" [TyCon "word" [], TyCon "option" [TyCon "bool" []]]]]
              Nothing
              (Just [TyCon "comptime" [TyCon "word" []]])
          ),
      testCase "nested generic closers remain separate tokens" $
        parsesAs
          typeP
          "option<option<word>>"
          (TyCon "option" [TyCon "option" [TyCon "word" []]]),
      testCase "tuple and qualified types allow trivia and trailing commas" $
        parsesAs
          typeP
          "(moduleA . /* segment */ Value<word,>, bool,)"
          (pairTy (TyCon (QualName "moduleA" "Value") [TyCon "word" []]) (TyCon "bool" [])),
      testCase "empty and singleton tuple types" $ do
        parsesAs typeP "()" (TyCon "()" [])
        parsesAs typeP "(word,)" (TyCon "word" []),
      testCase "generic parameters allow a trailing comma" $
        parsesAs typeParamsP "<a, b,>" [TyCon "a" [], TyCon "b" []],
      testCase "where clause accepts parenthesized predicate list" $
        parsesAs
          whereClauseP
          "where (a: Eq, (a, b): Convert<c,>,)"
          [ InCls "Eq" (TyCon "a" []) [],
            InCls "Convert" (pairTy (TyCon "a" []) (TyCon "b" [])) [TyCon "c" []]
          ],
      testCase "predicate class must be unqualified" $
        parseFails predP "a: moduleA.Eq",
      testCase "named and lambda parameter requirements" $ do
        parsesAs namedParamP "comptime value: word" (Typed True "value" (TyCon "word" []))
        parsesAs lambdaParamP "value" (Untyped False "value")
        parseFails namedParamP "value"
        parseFails lambdaParamP "comptime value"
        parseFails namedParamP "value: comptime<word>"
        parseFails lambdaParamP "comptime: word",
      testCase "removed type spellings are rejected" $
        mapM_
          (parseFails typeP)
          [ "word[]",
            "word[2]",
            "word memory",
            "word storage",
            "word calldata",
            "function(word) internal returns (word)",
            "function(word) external",
            "word -> bool",
            "mapping<word, bool>",
            "mapping",
            "option<>",
            "comptime word"
          ],
      testCase "contextual keywords remain identifiers" $
        mapM_
          (\name -> parsesAs identifier name name)
          [ "enum",
            "trait",
            "impl",
            "comptime",
            "returns",
            "where",
            "from",
            "hiding",
            "mapping",
            "while",
            "interface",
            "library",
            "struct",
            "alias",
            "internal",
            "external",
            "private",
            "pure",
            "view",
            "revert",
            "unchecked"
          ],
      testCase "hard keywords and Yul-only identifiers are rejected" $
        mapM_
          (parseFails identifier)
          [ "data",
            "class",
            "forall",
            "instance",
            "lam",
            "function",
            "true",
            "false",
            "_",
            "_value",
            "$value",
            "value$tail",
            "foo-bar"
          ],
      testCase "Unicode letters and numbers are accepted in identifiers" $
        parsesAs identifier "λ二2_" "λ二2_",
      testCase "keywords do not split longer lexer tokens" $ do
        parseFails (keyword "let") "let$x"
        parseFails (keyword "if") "if-then",
      testCase "operators match complete tokens" $ do
        parseFails (symbol "=") "=="
        parseFails (symbol "=") "=>"
        parseFails (symbol "*") "*="
        parseFails (symbol ":") ":="
        parsesAs (symbol ">" *> symbol ">") ">>" ">",
      testCase "supported string escapes and nested comments" $ do
        parsesAs stringLit "/* a /* b */ c */ \"hello\\n\\t\\\"\\\\\"" "hello\n\t\"\\"
        parseFails stringLit "\"bad\\r\""
        parseFails stringLit "\"bad\\q\""
        parseFails stringLit "/* unterminated"
    ]

parsesAs :: (Show a, Eq a) => Parser a -> String -> a -> Assertion
parsesAs parser source expected =
  case runParserE (sc *> parser <* eof) "<new-syntax>" source of
    Left err -> assertFailure err
    Right actual -> assertEqual source expected actual

parseFails :: (Show a) => Parser a -> String -> Assertion
parseFails parser source =
  case runParserE (sc *> parser <* eof) "<new-syntax>" source of
    Left _ -> pure ()
    Right actual -> assertFailure ("unexpectedly parsed " ++ source ++ ": " ++ show actual)
