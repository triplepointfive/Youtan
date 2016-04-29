{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Parses strings into a set of tokens using the given rules.
module Youtan.Lexical.Tokenizer
( tokenize
, tokenizeT
, Rules
) where

import Control.Monad.State
import Control.Monad.Except
import Data.List ( inits )

import Youtan.Regex.NDFM ( match )

-- | Represents a set of parsing rules.
type Rules a = [ ( String, String -> a ) ]

-- | Applies a set of parsing rules to split the input string
-- into a set of tokens.
tokenize :: MonadPlus m => Rules a -> String -> m a
tokenize r s = either error id ( tokenizeT r s )
  -- evalState ( parse mzero rules )

-- | Monadic version of 'tokenize' wrappers errors into a monad.
tokenizeT :: ( Wrapper w, MonadPlus m ) => Rules a -> String -> w ( m a )
tokenizeT rules = evalStateT ( parse mzero rules )

-- | Shortcut for types.
type P w a = StateT String w a

-- | Shortcut for monad error.
type Wrapper w = MonadError String w

-- | Applies a regex to an input and returns the longest match.
tryParse :: Wrapper w => String -> P w ( Maybe String )
tryParse regex = do
  inp <- get
  -- TODO: Need another function, that would stop matching as soon as
  -- DFM starts failing.
  -- E.g. for regex "a*" no reason to try all inits for "aab... 1K chars more".
  let x = filter ( match regex ) ( inits inp )
      y = last x
  if null x || null y
  then return Nothing
  else modify ( drop ( length y ) ) >> return ( Just y )

-- | Applies rules one by one until one matches or
choice :: Wrapper w => Rules a -> P w a
choice [] = ( take 10 <$> get ) >>= throwError . (++) "All options failed "
choice ( ( r, f ) : xs ) = do
  x <- tryParse r
  case x of
    Just s -> return ( f s )
    Nothing -> choice xs

-- | Checks if the whole input is consumed.
done :: Wrapper w => P w Bool
done = null <$> get

-- | Splits the input into a set of tokens.
parse :: ( MonadPlus m, Wrapper w ) => m a -> Rules a -> P w ( m a )
parse stack rules = do
  status <- done
  if status
  then return stack
  else do
    x <- choice rules
    parse ( mplus stack ( pure x ) ) rules
