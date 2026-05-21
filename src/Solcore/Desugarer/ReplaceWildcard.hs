module Solcore.Desugarer.ReplaceWildcard
  ( replaceWildcard,
    replaceWildcardTopDecls,
  )
where

import Control.Monad.State
import Data.Generics
import Solcore.Frontend.Syntax

-- replacing wildcards by fresh pattern variables

replaceWildcard :: CompUnit Name -> CompUnit Name
replaceWildcard = replaceWildcards

replaceWildcardTopDecls :: [TopDecl Name] -> [TopDecl Name]
replaceWildcardTopDecls = replaceWildcards

replaceWildcards :: (Data a) => a -> a
replaceWildcards c = fst (runState (replace c) 0)

replace :: (Data a) => a -> State Int a
replace c = everywhereM (mkM replacePat) c

replacePat :: Pat Name -> State Int (Pat Name)
replacePat PWildcard =
  do
    i <- inc
    let n = Name ("var_" ++ show i)
    pure (PVar n)
replacePat (PCon n ps) =
  PCon n <$> mapM replacePat ps
replacePat p = pure p

inc :: State Int Int
inc = do
  n <- get
  put (n + 1)
  pure n
