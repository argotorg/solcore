{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language.Core(Object(..))
import Language.Core.Parser(parseObject)
import Solcore.Frontend.Syntax.Name  -- FIXME: move Name to Common
import Common.Pretty -- (Doc, Pretty(..), nest, render)
import Builtins(yulBuiltins)
import Compress
import TM
import Translate
import Language.Yul
import qualified Options
import Options(parseOptions)
import Control.Monad(when)
import Data.String(fromString)


main :: IO ()
main = do
    options <- parseOptions
    -- print options
    let filename = Options.input options
    src <- readFile filename
    let (Object name code inners) = parseObject filename src
    let oCompress = Options.compress options
    when oCompress $ do
        putStrLn "Compressing sums"
    let source = if oCompress then compress code else code
    generatedYul <- runTM options (translateStmts source)
    let withDeployment = not (Options.runOnce options)
    let doc = if Options.wrap options
        then wrapInSol (Name name) generatedYul
        else wrapInObject (Name name) withDeployment generatedYul
    putStrLn ("writing output to " ++ Options.output options)
    writeFile (Options.output options) (render doc)

-- wrap in a Yul object with the given name
wrapInObject :: Name -> Bool -> [YulStmt] -> Doc
wrapInObject name deploy yul
  | deploy    = ppr nested
  | otherwise = ppr runtime
  where
    nested = YulObject "Deployable" deploycode [InnerObject runtime]
    runtime = YulObject (show name) (YulCode stmts) []
    cname = yulString (show name)
    stmts = yul ++ retcode
    retcode =
      [ calls "mstore" [yulInt 0, YIdent "_mainresult"]
      , calls "return" [yulInt 0, yulInt 32]
      ]
    deploycode = YulCode
      [ calls "mstore" [yulInt 64, YCall "memoryguard" [yulInt 128]]
      , ylva "memPtr" (YCall "mload" [yulInt 64])
      -- call constructor here
      , calls "datacopy" [yulInt 0, dataoffset, datasize]
      , calls "return" [yulInt 0, datasize]
      ]
    calls f args = YExp (YCall f args)
    ylva x e = YLet [Name x] (Just e)
    datasize = YCall "datasize"[cname]
    dataoffset = YCall "dataoffset"[cname]

{- | wrap a Yul chunk in a Solidity function with the given name
   assumes result is in a variable named "_result"
-}
wrapInSol :: Name -> [YulStmt] -> Doc
wrapInSol name yul = wrapInContract name "wrapper()" wrapper
    where
        wrapper = wrapInSolFunction "wrapper" (yulBuiltins <> yul)

wrapInSolFunction :: Name -> [YulStmt] -> Doc
wrapInSolFunction name yul =
  text "function" <+> ppr name <+> prettyargs <+> text " public returns (uint256 _wrapresult)" <+> lbrace
  $$ nest 2 assembly
  $$ rbrace
  where
    yul' = yul <> [YAssign1 "_wrapresult" (YIdent "_mainresult")]
    assembly = text "assembly" <+> braces (nest 2 prettybody)
    prettybody = vcat (map ppr yul')
    prettyargs = parens empty

wrapInContract ::  Name -> Name -> Doc -> Doc
wrapInContract name entry body = empty
  $$ text "// SPDX-License-Identifier: UNLICENSED"
  $$ text "pragma solidity ^0.8.23;"
  $$ text "import {console,Script} from \"lib/stdlib.sol\";"
  $$ text "contract" <+> ppr name <+> text "is Script"<+> lbrace
  $$ nest 2 run
  $$ nest 2 body
  $$ rbrace
  where
    run = text "function run() public" <+> lbrace
      $$ nest 2 (text "console.log(\"RESULT --> \","<+> ppr entry >< text ");")
      $$ rbrace $$ text ""
