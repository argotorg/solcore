module Solcore.Frontend.TypeInference.NameSupply where 

import Solcore.Frontend.Syntax.Name

type NameSupply = [Name]

namePool :: NameSupply
namePool = Name <$> (names ++ addNumbers names [1..])
  where
    names = map wrap ['a' .. 'z']
    wrap x = [x]


addNumbers :: [String] -> [Int] -> [String]
addNumbers xs ys 
  = do
      x <- xs 
      y <- ys 
      return (x ++ show y)

newName :: NameSupply -> (Name, NameSupply)
newName (x : xs) = (x, xs)
