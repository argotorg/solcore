{-# LANGUAGE OverloadedStrings #-}
module Builtins(yulBuiltins, revertStmt) where
import Data.String
import Language.Yul

yulBuiltins :: Yul
yulBuiltins = Yul []

revertStmt :: String -> [YulStmt]
revertStmt s =  [ YExp $ YCall "mstore" [yulInt 0, YLit (YulString s)]
                , YExp $ YCall "revert" [yulInt 0, yulIntegral (length s)]
                ]

{-
poisonBuiltin :: [YulStmt]
poisonBuiltin =
    [ YFun "$poison" [] (YReturns ["_dummy"]) (revertStmt "Dying from poison!") ]
-}
