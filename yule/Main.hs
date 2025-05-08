{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language.Core(Contract(..))
import Language.Core.Parser(parseContract)
import Solcore.Frontend.Syntax.Name  -- FIXME: move Name to Common
import Solcore.Frontend.Pretty.Name
import Common.Pretty -- (Doc, Pretty(..), nest, render)
import qualified Common.Pretty as Pretty
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
        putStrLn (render (nest 2 (ppr source)))
    generatedYul <- runTM options (translateStmts source)
    let name = fromString (ccName coreContract)

    let doc = if Options.wrap options
        then wrapInSol name generatedYul
        else wrapInObject name generatedYul
    putStrLn (render doc)
    putStrLn ("writing output to " ++ Options.output options)
    writeFile (Options.output options) (render doc)

-- wrap in a Yul object with the given name
wrapInObject :: Name -> Yul -> Doc
wrapInObject name yul = ppr object where
    object = YulObject (show name) (YulCode stmts) []
    stmts = yulStmts yul ++ retcode
    retcode =
      [ call "mstore" [yulInt 0, YIdent "_mainresult"]
      , call "return" [yulInt 0, yulInt 32]
      ]
    call f args = YExp (YCall f args)

{- | wrap a Yul chunk in a Solidity function with the given name
   assumes result is in a variable named "_result"
-}

wrapInSol name yul = wrapInContract name "wrapper()" wrapper
    where
        wrapper = wrapInSolFunction "wrapper" (yulBuiltins <> yul)

wrapInSolFunction :: Name -> Yul -> Doc
wrapInSolFunction name yul =
  text "function" <+> ppr name <+> prettyargs <+> text " public pure returns (uint256 _wrapresult)" <+> lbrace
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
    run = text "function run() public view" <+> lbrace
      $$ nest 2 (text "console.log(\"RESULT --> \","<+> ppr entry >< text ");")
      $$ rbrace $$ text ""
