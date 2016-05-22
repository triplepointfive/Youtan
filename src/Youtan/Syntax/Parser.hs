-- | Simple parser combinator.
module Youtan.Syntax.Parser
( Parser( .. )
, (<|>)
, (!)
, (<<)
, chainl
, joins
, many
, oneOf
, opt
, pad
, runParser
, satisfy
, skip
, some
, term
) where

import Control.Applicative
import Control.Monad

-- | Parser is a function that transforms a set of tokens
-- into a new structure and a list of unused tokens.
newtype Parser a b = Parser { _parse :: [ a ] -> [( b, [ a ] )] }

-- | Applies a parser to a list of tokens. Fails if parser
-- couldn't consume entire input.
runParser :: Parser a b -> [ a ] -> Either [ ( b, [ a ] ) ] b
runParser m s =
  case _parse m s of
    [(res, [])] -> Right res
    [(x, rs)]   -> Left [ ( x, rs ) ]
    _           -> error "Parser error."

instance Functor ( Parser a ) where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative ( Parser a ) where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance MonadPlus ( Parser a ) where
  mzero = failure
  mplus = combine

instance Alternative ( Parser a ) where
  empty = mzero
  (<|>) = option

-- | Applies a parser to unmatch input after first parser.
combine :: Parser a b -> Parser a b -> Parser a b
combine p q = Parser (\s -> _parse p s ++ _parse q s)

-- | Parser that fails to parse.
failure :: Parser a b
failure = Parser ( const [] )

-- | Returns the result of first parser if it matches,
-- otherwise the result of second parser.
option :: Parser a b -> Parser a b -> Parser a b
option  p q = Parser $ \s ->
  case _parse p s of
    []     -> _parse q s
    res    -> res

instance Monad ( Parser a ) where
  return = unit
  (>>=)  = bind

-- | Parser the consumes any token.
item :: Parser a a
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

-- | Combines parsers.
bind :: Parser a b -> ( b -> Parser a c ) -> Parser a c
bind p f = Parser $ \s -> concatMap (\(a, s') -> _parse (f a) s') ( _parse p s )

-- | Parser that builds specified value with no token consumed.
unit :: b -> Parser a b
unit a = Parser (\s -> [(a,s)])

-- | Builds a parser from the given predicate.
satisfy :: ( a -> Bool ) -> Parser a a
satisfy p = bind item $ \c -> if p c then unit c else Parser ( const [] )

oneOf :: Eq a => [ a ] -> Parser a a
oneOf s = satisfy ( `elem` s )

chainl :: Parser a b -> Parser a ( b -> b -> b ) -> b -> Parser a b
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a b -> Parser a (b -> b -> b) -> Parser a b
chainl1 p op = op >>= \f -> p >>= rest f
  where rest f a = ( p >>= rest f . f a  ) <|> return a

-- | Builds a parser that matches given term.
term :: Eq a => a -> Parser a ()
term t = void $ satisfy ( == t )

-- | Either matching a parser or consumes nothing.
opt :: Eq a => a -> Parser a ()
opt t = term t <|> return ()

-- | Return the result of second parser, ignoring result of surrounding ones.
pad :: Parser a b -> Parser a c -> Parser a d -> Parser a c
pad l m r = void l >> m << void r

-- | Returns a list of matches, separated with first parser. List could be empty.
joins :: Parser a c -> Parser a b -> Parser a [ b ]
joins d p = ( do
  x <- p
  xs <- many ( d >> p )
  return ( x : xs )
            ) <|> return []

-- | Ignores result of second parser and return the result of first one.
skip :: Parser a b -> Parser a c -> Parser a b
skip rule ignore = rule >>= \ v -> ignore >> return v

-- | Alias for 'skip'.
(<<) :: Parser a b -> Parser a c -> Parser a b
(<<) = skip

infixl 3 !

-- | Either combinator.
(!) :: Parser a b -> Parser a b -> Parser a b
(!) = option
