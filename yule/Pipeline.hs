{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pipeline (lower) where

import Builtins (yulBuiltins)
import Common.Pretty
import Compress
import Control.Monad (when)
import Language.Hull qualified as Hull
import Language.Yul
import Language.Yul.QuasiQuote
import Options (Options)
import Options qualified
import Solcore.Frontend.Syntax.Name
import TM
import Translate

-- | Lower a Hull object to Yul source text.
-- Handles compression, translation, deployment wrapping, and rendering.
-- Returns (objectName, yulSource).
lower :: Options -> Hull.Object -> IO (String, String)
lower options inputObject = do
  let oCompress = Options.compress options
  when oCompress $ do
    putStrLn "Compressing sums"
  let compObject =
        if oCompress
          then compress inputObject
          else inputObject
  yulPreobject@(YulObject yulName yulCode _) <- runTM options (translateObject compObject)
  let withDeployment = not (Options.runOnce options)
  let (finalName, doc) =
        if Options.wrap options
          then (yulName, wrapInSol (Name yulName) (ycStmts yulCode))
          else wrapInObject' withDeployment yulPreobject
  pure (finalName, render doc)

-- wrap in a Yul object with the given name, returning (finalName, doc)
wrapInObject' :: Bool -> YulObject -> (String, Doc)
wrapInObject' deploy yulo@(YulObject name code inners)
  | deploy    = let deployed = createDeployment yulo
                    YulObject dname _ _ = deployed
                in (dname, ppr deployed)
  | otherwise = (name, ppr (YulObject name (addRetCode code) inners))

addRetCode :: YulCode -> YulCode
addRetCode c = c <> retCode
  where
    retCode =
      YulCode
        [yulBlock|
    {
      mstore(0, _mainresult)
      return(0, 32)
    }
    |]

deployCode :: String -> Bool -> YulCode
deployCode _name withStart = YulCode $ go withStart
  where
    go True = [[yulStmt| usr$start() |]]
    go False = []

createDeployment :: YulObject -> YulObject
createDeployment (YulObject yulName yulCode [InnerObject (YulObject innerName innerCode [])]) =
  YulObject yulName yulCode' [yulInner']
  where
    yulCode' = yulCode <> deployCode innerName True
    yulInner' = InnerObject (YulObject innerName (addRetCode innerCode) [])
createDeployment (YulObject yulName yulCode []) =
  YulObject yulName' yulCode' [yulInner']
  where
    yulName' = yulName <> "Deploy"
    yulCode' = deployCode yulName False
    yulInner' = InnerObject (YulObject yulName (addRetCode yulCode) [])
createDeployment _ = error ("createDeployment not implemented for this type of object")

-- | wrap a Yul chunk in a Solidity function with the given name
--   assumes result is in a variable named "_result"
wrapInSol :: Name -> [YulStmt] -> Doc
wrapInSol name yul = wrapInContract name "wrapper()" wrapper
  where
    wrapper = wrapInSolFunction "wrapper" (yulBuiltins <> yul)

wrapInSolFunction :: Name -> [YulStmt] -> Doc
wrapInSolFunction name yul =
  text "function"
    <+> ppr name
    <+> prettyargs
    <+> text " public returns (uint256 _wrapresult)"
    <+> lbrace
    $$ nest 2 assembly
    $$ rbrace
  where
    yul' = yul <> pure [yulStmt| _wrapresult := _mainresult |]
    assembly = text "assembly" <+> braces (nest 2 prettybody)
    prettybody = vcat (map ppr yul')
    prettyargs = parens empty

wrapInContract :: Name -> Name -> Doc -> Doc
wrapInContract name entry body =
  empty
    $$ text "// SPDX-License-Identifier: UNLICENSED"
    $$ text "pragma solidity ^0.8.23;"
    $$ text "import {console,Script} from \"lib/stdlib.sol\";"
    $$ text "contract"
    <+> ppr name
    <+> text "is Script"
    <+> lbrace
    $$ nest 2 run
    $$ nest 2 body
    $$ rbrace
  where
    run =
      text "function run() public"
        <+> lbrace
        $$ nest 2 (text "console.log(\"RESULT --> \"," <+> ppr entry >< text ");")
        $$ rbrace
        $$ text ""
