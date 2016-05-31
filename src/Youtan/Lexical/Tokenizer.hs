{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- | Parses strings into a set of tokens using the given rules.
module Youtan.Lexical.Tokenizer
( tokenize
, tokenizeDrops
, tokenizeT
, tokenizeTDrops
, Rules
, Wrapper
) where

import Control.Monad.State
import Control.Monad.Except

import Youtan.Regex.DFM ( DFM, fromString, longestMatchDFM )

-- | Represents a set of parsing rules.
type Rules a = [ ( String, String -> a ) ]

-- | Same as 'Rules' with prepared 'DFM' per each rule.
data ParsingRules a
  = ParsingRules
    { dfm        :: !DFM
    , matches    :: ![ String -> a ]
    , rulesCount :: !Int
    }

-- | Build 'DFM' per each rule.
buildParingRules :: Rules a -> [ String ] -> ParsingRules a
buildParingRules rules drops
  = ParsingRules
    { dfm        = mconcat ( map fromString ( map fst rules ++ drops  ) )
    , matches    = map snd rules
    , rulesCount = length rules
    }

-- | Applies a set of parsing rules to split the input string
-- into a set of tokens.
tokenize :: MonadPlus m => Rules a -> String -> m a
tokenize r = tokenizeDrops r []

-- | Monadic version of 'tokenize' wrappers errors into a monad.
tokenizeT :: ( Wrapper w, MonadPlus m ) => Rules a -> String -> w ( m a )
tokenizeT rules = tokenizeTDrops rules []

-- | Applies a set of parsing rules to split the input string
-- into a set of tokens.
tokenizeDrops :: MonadPlus m => Rules a -> [ String ] -> String -> m a
tokenizeDrops r d s = either error id ( tokenizeTDrops r d s )

-- | Monadic version of 'tokenize' wrappers errors into a monad.
tokenizeTDrops :: ( Wrapper w, MonadPlus m ) => Rules a -> [ String ] -> String -> w ( m a )
tokenizeTDrops rules drops = evalStateT ( parse mzero ( buildParingRules rules drops ) )

-- | Shortcut for types.
type P w a = StateT String w a

-- | Shortcut for monad error.
type Wrapper w = MonadError String w

-- | Chooses the longest match and applis related modifier to it.
choice :: Wrapper w => ParsingRules a -> P w ( Maybe a )
choice ParsingRules{..} = do
  inp <- get
  case longestMatchDFM dfm inp of
    Nothing -> failMessage
    Just ( x, mID ) ->
      if x > 0
      then do
        modify ( drop x )
        return $ if use mID
          then Just ( ( matches !! mID ) ( take x inp ) )
          else Nothing
      else failMessage
  where
    use mDI = mDI < rulesCount
    failMessage = ( take 10 <$> get ) >>= throwError . (++) "All options failed "

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
    case x of
      Just y  -> parse ( mplus stack ( pure y ) ) rules
      Nothing -> parse stack rules
