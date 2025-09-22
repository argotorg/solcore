{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language.Core(Contract(..))
import Language.Core.Parser(parseContract)
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
    let coreContract = parseContract filename src
    let core = ccStmts coreContract
    when (Options.verbose options) $ do
        putStrLn "/* Core:"
        putStrLn (render (nest 2 (ppr coreContract)))
        putStrLn "*/"
    let oCompress = Options.compress options
    let source = if oCompress then compress core else core
    when oCompress $ do
        putStrLn "Compressing sums"
    generatedYul <- runTM options (translateStmts source)
    let name = fromString (ccName coreContract)
    let withDeployment = not (Options.runOnce options)
    let doc = if Options.wrap options
        then wrapInSol name generatedYul
        else wrapInObject name withDeployment generatedYul
    putStrLn ("writing output to " ++ Options.output options)
    writeFile (Options.output options) (render doc)

-- wrap in a Yul object with the given name
wrapInObject :: Name -> Bool -> Yul -> Doc
wrapInObject name deploy yul
  | deploy    = ppr nested
  | otherwise = ppr runtime
  where
    nested = YulObject "Deployable" deploycode [InnerObject runtime]
    runtime = YulObject (show name) (YulCode stmts) []
    cname = yulString (show name)
    stmts = yulStmts yul ++ retcode
    retcode =
      [ calls "mstore" [yulInt 0, YIdent "_mainresult"]
      , calls "return" [yulInt 0, yulInt 32]
      ]
    deploycode = YulCode
      [ calls "datacopy" [yulInt 0, dataoffset, datasize]
      , calls "return" [yulInt 0, datasize]
      ]
    calls f args = YExp (YCall f args)
    datasize = YCall "datasize"[cname]
    dataoffset = YCall "dataoffset"[cname]

{- | wrap a Yul chunk in a Solidity function with the given name
   assumes result is in a variable named "_result"
-}
wrapInSol :: Name -> Yul -> Doc
wrapInSol name yul = wrapInContract name "wrapper()" wrapper
    where
        wrapper = wrapInSolFunction "wrapper" (yulBuiltins <> yul)

wrapInSolFunction :: Name -> Yul -> Doc
wrapInSolFunction name yul =
  text "function" <+> ppr name <+> prettyargs <+> text " public returns (uint256 _wrapresult)" <+> lbrace
  $$ nest 2 assembly
  $$ rbrace
  where
    yul' = yul <> Yul [YAssign1 "_wrapresult" (YIdent "_mainresult")]
    assembly = text "assembly" <+> lbrace
      $$ nest 2 (ppr yul')
      $$ rbrace
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
