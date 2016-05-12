-- | Base functionallity, shared within DFM and NDFM.
module Youtan.Regex.FM where

import Data.Char ( isDigit, isAscii, isSpace )

import Youtan.Regex.Operators ( CharacterClass(..) )

-- | A literal in input string.
type Symbol = Char

-- | An input string.
type Input  = String

-- | Extension for char matching, allows to match with 'CharacterClass'.
data Matcher
  = Exact Symbol         -- ^ Single char.
  | Class CharacterClass -- ^ Any 'CharacterClass'.
  deriving ( Ord, Eq )

-- | For better display.
instance Show Matcher where
  show ( Exact ch ) = [ ch ]
  show ( Class chClass ) = show chClass

-- | Search for matching branches within a node. Includes brank branches.
matchState :: [ ( Matcher, a ) ] -> Symbol -> [ a ]
matchState branches char = map snd $ filter ( matches . fst ) branches
  where
    matches ( Exact x )    = x == char
    matches ( Class Dot )  = True
    matches ( Class Word ) = any ( \f -> f char ) [ isAscii, isDigit, (==) '_' ]
    matches ( Class Digit )      = isDigit char
    matches ( Class Whitespace ) = isSpace char
    matches ( Class ( CharGroup group ) ) = char `elem` group
    matches ( Class ( None chClass ) ) = not $ matches ( Class chClass )

-- | ID of a node, must be uniq within 'DFM'.
type StateID = Int

-- | Just some ID to start off.
initID :: StateID
initID = 0

-- | Returns next ID in a chain.
nextFreeID :: StateID -> StateID
nextFreeID = succ

class FM a where
  -- | Checks whether 'FM' accepts given string.
  matchFM :: a -> String -> Bool
