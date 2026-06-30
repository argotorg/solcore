module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Solcore.Pipeline.SolcorePipeline

main :: IO ()
main = do
  setLocaleEncoding utf8
  pipeline
