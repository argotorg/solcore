{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Hull.Parser (parseObject)
import Options (parseOptions)
import Options qualified
import Pipeline (lower)

main :: IO ()
main = do
  options <- parseOptions
  let filename = Options.input options
  src <- readFile filename
  let inputObject = parseObject filename src
  (_, yulText) <- lower options inputObject
  putStrLn ("writing output to " ++ Options.output options)
  writeFile (Options.output options) yulText
