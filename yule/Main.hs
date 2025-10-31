{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where
import Language.Core.Parser(parseObject)
import Solcore.Frontend.Syntax.Name  -- FIXME: move Name to Common
import Common.Pretty -- (Doc, Pretty(..), nest, render)
import Builtins(yulBuiltins)
import Compress
import Language.Yul
import Language.Yul.QuasiQuote
import qualified Options
import Options(parseOptions)
import TM
import Translate
import Control.Monad(when)


main :: IO ()
main = do
    options <- parseOptions
    -- print options
    let filename = Options.input options
    src <- readFile filename
    let inputObject = parseObject filename src
    let oCompress = Options.compress options
    when oCompress $ do
        putStrLn "Compressing sums"
    let compObject = if oCompress then compress inputObject
                                  else inputObject
    -- Yul "preobject" - lacking deployment code
    yulPreobject@(YulObject yulName yulCode _) <- runTM options (translateObject compObject)
    let withDeployment = not (Options.runOnce options)
    let doc = if Options.wrap options
        then wrapInSol (Name yulName) (ycStmts yulCode)
        else wrapInObject withDeployment yulPreobject
    putStrLn ("writing output to " ++ Options.output options)
    writeFile (Options.output options) (render doc)

-- wrap in a Yul object with the given name
wrapInObject :: Bool -> YulObject -> Doc
wrapInObject deploy yulo@(YulObject name code inners)
  | deploy    = ppr (createDeployment yulo)
  | otherwise = ppr (YulObject name (addRetCode code) inners)

addRetCode :: YulCode -> YulCode
addRetCode c = c <> retCode where
    retCode = YulCode [yulBlock|
    {
      mstore(0, _mainresult)
      return(0, 32)
    }
    |]

deployCode :: String -> Bool -> YulCode
deployCode name withConstructor = YulCode $ [yulBlock|
  {
    mstore(64, memoryguard(128))
    let memPtr := mload(64)
  }
  |]
  <> callConstructor withConstructor
  <> [yulBlock|
     { datacopy(0, `dataoffset`, `datasize`)
       return(0, `datasize`)
     } |]
  where
    cname = yulString name
    callConstructor True = pure [yulStmt| usr$constructor() |]
    callConstructor False = []
    datasize = [yulExp| datasize(${cname}) |] -- YCall "datasize"[cname]
    dataoffset = [yulExp| dataoffset(`cname`) |]

createDeployment :: YulObject -> YulObject
createDeployment (YulObject yulName yulCode [InnerObject(YulObject innerName innerCode [])])
  = YulObject yulName yulCode' [yulInner']
  where
    yulCode' = yulCode <> deployCode innerName True
    yulInner' = InnerObject (YulObject innerName (addRetCode innerCode) [])
createDeployment (YulObject yulName yulCode [])
  = YulObject yulName' yulCode' [yulInner'] where
    yulName' = yulName <> "Deploy"
    yulCode' = deployCode yulName False
    yulInner' = InnerObject (YulObject yulName (addRetCode yulCode) [])
createDeployment _ = error("createDeployment not implemented for this type of object")

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
    yul' = yul <> pure [yulStmt| _wrapresult := _mainresult |]
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
