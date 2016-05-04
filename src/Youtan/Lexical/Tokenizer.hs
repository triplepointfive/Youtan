{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Parses strings into a set of tokens using the given rules.
module Youtan.Lexical.Tokenizer
( tokenize
, tokenizeT
, Rules
, Wrapper
) where

import Control.Arrow ( first )
import Control.Monad.State
import Control.Monad.Except

import Youtan.Regex.DFM ( DFM, fromString, longestMatchDFM )

-- | Represents a set of parsing rules.
type Rules a = [ ( String, String -> a ) ]

-- | Same as 'Rules' with prepared 'DFM' per each rule.
type ParsingRules a = [ ( DFM, String -> a) ]

-- | Build 'DFM' per each rule.
buildParingRules :: Rules a -> ParsingRules a
buildParingRules = map ( first fromString )

-- | Applies a set of parsing rules to split the input string
-- into a set of tokens.
tokenize :: MonadPlus m => Rules a -> String -> m a
tokenize r s = either error id ( tokenizeT r s )

-- | Monadic version of 'tokenize' wrappers errors into a monad.
tokenizeT :: ( Wrapper w, MonadPlus m ) => Rules a -> String -> w ( m a )
tokenizeT rules = evalStateT ( parse mzero ( buildParingRules rules ) )

-- | Shortcut for types.
type P w a = StateT String w a

-- | Shortcut for monad error.
type Wrapper w = MonadError String w

-- | Applies a regex to an input and returns the longest match.
tryParse :: Wrapper w => DFM -> P w ( Maybe String )
tryParse dfm = do
  inp <- get
  case longestMatchDFM dfm inp of
    Nothing -> return Nothing
    Just x -> if x > 0 then modify ( drop x ) >> return ( Just ( take x inp ) )
                       else return Nothing

-- | Applies rules one by one until one matches or
choice :: Wrapper w => ParsingRules a -> P w a
choice [] = ( take 10 <$> get ) >>= throwError . (++) "All options failed "
choice ( ( dfm, f ) : xs ) = do
  x <- tryParse dfm
  case x of
    Just s -> return ( f s )
    Nothing -> choice xs

-- | Checks if the whole input is consumed.
done :: Wrapper w => P w Bool
done = null <$> get

-- | Splits the input into a set of tokens.
parse :: ( MonadPlus m, Wrapper w ) => m a -> ParsingRules a -> P w ( m a )
parse stack rules = do
  status <- done
  if status
  then return stack
  else do
    x <- choice rules
    parse ( mplus stack ( pure x ) ) rules
