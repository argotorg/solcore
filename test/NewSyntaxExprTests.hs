module NewSyntaxExprTests (newSyntaxExprTests) where

import Common.LightYear hiding (parse)
import Solcore.Frontend.Lexer.SolcoreLexer (braces, identifier, sc)
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.Patterns (patP)
import Solcore.Frontend.Parser.Stmt (bodyP, namedBodyP, stmtP)
import Solcore.Frontend.Syntax.SyntaxTree
import Test.Tasty
import Test.Tasty.HUnit

newSyntaxExprTests :: TestTree
newSyntaxExprTests =
  testGroup
    "new syntax expressions, statements, and patterns"
    [ testGroup "Unicode identifiers" $
        map (accepts identifier) ["résultat", "値", "α₂"],
      testGroup "invalid identifier starts" $
        map (rejects identifier) ["1value", "₂value", "_value"],
      testGroup "expression forms" $
        map
          (accepts expression)
          [ "[]",
            "[1, 2 + 3]",
            "[f(1), [2, 3]][0]",
            "@word",
            "@(word, bool)",
            "@array<word>",
            "~value",
            "!~value",
            "~~value",
            "~value.field[0]",
            "()",
            "(1,)",
            "(1, 2,)",
            "((1, 2,),)",
            "lam () { return; }",
            "lam (x,) -> word { return x; }",
            "lam (x, comptime y: word,) -> (word, bool) { return (x, true); }",
            "lam (x) -> comptime<word> { return x; }",
            "(lam (x) { return x; })(1)",
            "f(1)(2)[0].field",
            "a ? b ? c : d : e",
            "a ? b : c ? d : e",
            "true",
            ".false",
            ".Some(1)",
            "pkg.Enum.Some(1)"
          ],
      testGroup "removed or invalid expressions" $
        map
          (rejects expression)
          [ "x as word",
            "x: word",
            "2 ** 3",
            "x << 1",
            "x >> 1",
            "if true then 1 else 0",
            "-1",
            "+1",
            "[1,]",
            "f(1,)",
            ".Some(1,)",
            "lam (x) returns (word) { return x; }",
            "lam (comptime x) { return x; }",
            "lam (x: comptime<word>) { return x; }",
            "lam () { 1 }",
            "a < b < c",
            "a == b == c"
          ],
      testGroup "statement forms" $
        map
          (accepts stmtP)
          [ "let value;",
            "let value = 1;",
            "let value: word;",
            "let value: comptime<word> = 1;",
            "let comptime = 1;",
            "x *= 2;",
            "x /= 2;",
            "x ~=;",
            "x += 2;",
            "x -= 2;",
            "x ^= 2;",
            "x &= 2;",
            "x |= 2;",
            "x %= 2;",
            "for (; true; let x = 1) {}",
            "for (let i = 0, let j = 0; i < 10; i += 1, j *= 2) {}",
            "while (true) { break; }",
            "return;",
            "return [];",
            "match (x,) { case (1,) {} default {} }",
            "match (x, y,) { case (a, b,) {} default {} }",
            "match (x, y) { case ((a, b)) {} }",
            "match (x) { case (a, b,) {} }",
            "match (x) { default {} }",
            "match (x) { case comptime (lam (x) { return x; })(1) {} }"
          ],
      testGroup "removed or invalid statements" $
        map
          (rejects stmtP)
          [ "let comptime value: word = 1;",
            "let (a, b) = (1, 2);",
            "let (a, b): (word, word) = (1, 2);",
            "let value := 1;",
            "unchecked {}",
            "if true {}",
            "while true {}",
            "x *= 2",
            "x ~= 2;",
            "x ~=",
            "for (;; ) {}",
            "match (x) {}",
            "match (x) { default {} case _ {} }",
            "match (x) { default {} default {} }",
            "match (x, y) { case pair(a, b) {} }",
            "match (x, y) { case (pair(a, b)) {} }",
            "match (x, y) { case (a,) {} }",
            "match x { | _ => return 1; }",
            "if (true) { f() }",
            "{ f() }",
            "match (x) { case _ { f() } }"
          ],
      testGroup "pattern forms" $
        map
          (accepts patP)
          [ "_",
            "true",
            "false",
            ".true",
            ".Some(x)",
            "pkg.E.Some(x)",
            "()",
            "(x,)",
            "(x, y,)",
            "((x, y,),)",
            "0xff",
            "\"label\"",
            "comptime 1 + 2"
          ],
      testGroup "invalid patterns" $
        map (rejects patP) [".Some()", "Some()", "Some(x,)", ".Some(x,)", "[x, y]"],
      testGroup "source assembly forms" $
        map
          (accepts stmtP)
          [ "assembly { let _ := 1; let $flag := true; let fallback := false }",
            "assembly { function f(x, y,) -> first, second { first, second := g(x, y,); } }",
            "assembly { return(0, 32,); }",
            "assembly { for {} true {} { continue; break; } }",
            "assembly { switch value case 0 {} case 1 {} default {} }",
            "assembly { let text := \"`not a template`\"; }",
            "assembly { let text := \"${notATemplate}\"; }",
            "assembly { let x := $ { template } }",
            "assembly { let _a$2 := f(1,); { f(); }; }",
            "assembly { let 値 := 1; let _α₂ := 値; let $résultat := _α₂; }"
          ],
      testGroup "invalid source assembly" $
        map
          (rejects stmtP)
          [ "assembly { let x := `template` }",
            "assembly { let x := ${template} }",
            "assembly { switch x }",
            "assembly { switch x default {} }",
            "assembly { let x := \"invalid\\r\" }",
            "assembly { let function := 1 }",
            "assembly { let public := 1 }",
            "assembly { let true := 1 }",
            "assembly { let x := return(0, 1) }",
            "assembly { function f(x,) -> {} }",
            "assembly { function f() -> x, {} }",
            "assembly { let x, := f() }",
            "assembly { x, := f() }",
            "assembly { f();; }",
            "assembly { let 1value := 0 }"
          ],
      testGroup "named function tail expressions" $
        map
          (accepts (braces namedBodyP))
          ["{ 1 }", "{ f(); g() }", "{ let x = 1; x }", "{ return 1; }", "{}"],
      testGroup "invalid function tail placement" $
        map
          (rejects (braces namedBodyP))
          [ "{ f() g(); }",
            "{ if (true) { f() } }",
            "{ { f() } }",
            "{ x = 1 }",
            "{ return 1 }",
            "{ let f = lam () { 1 }; f() }"
          ],
      testCase "bitwise complement binds before multiplication" $
        case parse expression "~x * y + z & w" of
          Right (ExpBAnd (ExpPlus (ExpTimes (ExpBNot (ExpVar Nothing "x")) (ExpVar Nothing "y")) (ExpVar Nothing "z")) (ExpVar Nothing "w")) -> pure ()
          result -> assertFailure (show result),
      testCase "comptime local type sets its evaluation mode" $
        case parse stmtP "let x: comptime<word> = 1;" of
          Right (Let True "x" (Just (TyCon "word" [])) (Just (Lit (IntLit 1)))) -> pure ()
          result -> assertFailure (show result),
      testCase "tail expression becomes a return" $
        case parse (braces namedBodyP) "{ f(); g() }" of
          Right [StmtExp (ExpName Nothing "f" []), Return (ExpName Nothing "g" [])] -> pure ()
          result -> assertFailure (show result),
      testCase "multiplication, division and complement preserve source assignments" $
        case parse bodyP "x *= 2; x /= 3; x ~=;" of
          Right [StmtTimesEq _ _, StmtDivideEq _ _, StmtBNotEq _] -> pure ()
          result -> assertFailure (show result)
    ]

expression :: Parser Exp
expression = exprP bodyP

parse :: Parser a -> String -> Either String a
parse parser = runParserE (sc *> parser <* eof) "<new syntax>"

accepts :: Parser a -> String -> TestTree
accepts parser source = testCase source $
  case parse parser source of
    Right _ -> pure ()
    Left err -> assertFailure err

rejects :: (Show a) => Parser a -> String -> TestTree
rejects parser source = testCase source $
  case parse parser source of
    Left _ -> pure ()
    Right value -> assertFailure ("unexpectedly accepted: " ++ show value)
