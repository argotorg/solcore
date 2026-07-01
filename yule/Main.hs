{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

-- FIXME: move Name to Common
-- (Doc, Pretty(..), nest, render)

import Common.Pretty
import Control.Monad (unless, when)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Language.Hull.Compress
import Language.Hull.Parser (parseObject)
import Language.Hull.TcEnv (emptyHullTcEnv)
import Language.Hull.TcMonad (runHullTcM)
import Language.Hull.ToYul.Assemble (wrapInObject)
import Language.Hull.ToYul.Options (parseOptions)
import Language.Hull.ToYul.Options qualified as Options
import Language.Hull.ToYul.TM
import Language.Hull.ToYul.Translate
import Language.Hull.TypeCheck (checkObject)
import Language.Yul
import Language.Yul.Builtins (yulBuiltins)
import Language.Yul.QuasiQuote
import Solcore.Frontend.Syntax.Name
import System.Exit (exitFailure)

main :: IO ()
main = do
  setLocaleEncoding utf8
  options <- parseOptions
  -- print options
  let filename = Options.input options
  src <- readFile filename
  let inputObject = parseObject filename src
  let oCompress = Options.compress options
  when oCompress $ do
    putStrLn "Compressing sums"
  let compObject =
        if oCompress
          then compress inputObject
          else inputObject
  -- Hull/Yul type checking (skipped with --no-typecheck)
  unless (Options.noTypeCheck options) $ do
    result <- runHullTcM (checkObject compObject) emptyHullTcEnv
    case result of
      Left err -> do
        putStrLn ("Type error:\n" ++ err)
        exitFailure
      Right () -> pure ()
  -- Yul "preobject" - lacking deployment code
  yulPreobject@(YulObject yulName yulCode _) <- runTM options (translateObject compObject)
  let withDeployment = not (Options.runOnce options)
  let doc =
        if Options.wrap options
          then wrapInSol (Name yulName) (ycStmts yulCode)
          else wrapInObject withDeployment yulPreobject
  putStrLn ("writing output to " ++ Options.output options)
  writeFile (Options.output options) (render doc)

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
