-- | Parses strings into a set of tokens using the given rules.
module Youtan.Lexical.Tokenizer
( tokenize
, Rules
) where

import Control.Monad.State
import Data.List ( inits )

import Youtan.Regex.DFM ( match )

-- | Represents a set of parsing rules.
type Rules a = [ ( String, String -> a ) ]

-- | Applies a set of parsing rules to split the input string
-- into a set of tokens.
tokenize :: MonadPlus m => Rules a -> String -> m a
tokenize rules = evalState ( parse mzero rules )

-- | Shortcut for types.
type P a = State String a

-- | Applies a regex to an input and returns the longest match.
tryParse :: String -> P ( Maybe String )
tryParse regex = do
  inp <- get
  let x = filter ( match regex ) ( inits inp )
      y = last x
  if null x
  then return Nothing
  else modify ( drop ( length y ) ) >> return ( Just y )

-- | Applies rules one by one until one matches or
choice :: Rules a -> P a
choice [] = error "All options failed"
choice ( ( r, f ) : xs ) = do
  x <- tryParse r
  case x of
    Just s -> return ( f s )
    Nothing -> choice xs

-- | Checks if the whole input is consumed.
done :: P Bool
done = null <$> get

-- | Splits the input into a set of tokens.
parse :: MonadPlus m => m a -> Rules a -> P ( m a )
parse stack rules = do
  status <- done
  if status
  then return stack
  else do
    x <- choice rules
    parse ( mplus stack ( pure x ) ) rules
