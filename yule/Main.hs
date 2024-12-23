{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language.Core(Contract(..))
import Language.Core.Parser
import Common.Pretty(Pretty(..), nest, render)
import Builtins(yulBuiltins)
import Compress
import TM
import Translate
import Language.Yul(wrapInSolFunction, wrapInContract)
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
        when (Options.verbose options) $ putStrLn (render (nest 2 (ppr source)))
    generatedYul <- runTM options (translateStmts source)
    let fooFun = wrapInSolFunction "wrapper" (yulBuiltins <> generatedYul)
    let doc = wrapInContract (fromString (ccName coreContract)) "wrapper()" fooFun
    -- putStrLn (render doc)
    putStrLn ("writing output to " ++ Options.output options)
    writeFile (Options.output options) (render doc)
