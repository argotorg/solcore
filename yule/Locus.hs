module Locus where
import Data.String
{-
Location tree with addresses a:
- location for Int is a single cell
- location for pair is a pair of locations for components
- location for sum is a location for tag and locations for payload
-}
data LocTree a
    = LocWord Integer -- int literal
    | LocBool Bool    -- bool literal
    | LocStack a      -- stack location
    | LocNamed String -- named location (e.g. argument/result)
    | LocSeq [LocTree a] -- sequence of locations
    | LocEmpty Int    -- empty location of given size
    deriving (Eq, Show)

pattern LocPair a b = LocSeq [a, b]
pattern LocUnit = LocSeq []

type Location = LocTree Int

stkLoc :: IsString name => Int -> name
stkLoc i = fromString("_v" ++ show i)