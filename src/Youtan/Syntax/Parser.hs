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

newtype Parser a b = Parser { _parse :: [ a ] -> [( b, [ a ] )] }

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

combine :: Parser a b -> Parser a b -> Parser a b
combine p q = Parser (\s -> _parse p s ++ _parse q s)

failure :: Parser a b
failure = Parser ( const [] )

option :: Parser a b -> Parser a b -> Parser a b
option  p q = Parser $ \s ->
  case _parse p s of
    []     -> _parse q s
    res    -> res

instance Monad ( Parser a ) where
  return = unit
  (>>=)  = bind

item :: Parser a a
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

bind :: Parser a b -> ( b -> Parser a c ) -> Parser a c
bind p f = Parser $ \s -> concatMap (\(a, s') -> _parse (f a) s') ( _parse p s )

unit :: b -> Parser a b
unit a = Parser (\s -> [(a,s)])

satisfy :: ( a -> Bool ) -> Parser a a
satisfy p = bind item $ \c -> if p c then unit c else Parser ( const [] )

oneOf :: Eq a => [ a ] -> Parser a a
oneOf s = satisfy ( `elem` s )

chainl :: Parser a b -> Parser a ( b -> b -> b ) -> b -> Parser a b
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a b -> Parser a (b -> b -> b) -> Parser a b
chainl1 p op = op >>= \f -> p >>= rest f
  where rest f a = ( p >>= rest f . f a  ) <|> return a

term :: Eq a => a -> Parser a ()
term t = void $ satisfy ( == t )

opt :: Eq a => a -> Parser a ()
opt t = term t <|> return ()

pad :: Parser a b -> Parser a c -> Parser a d -> Parser a c
pad l m r = void l >> m << void r

joins :: Parser a c -> Parser a b -> Parser a [ b ]
joins d p = ( do
  x <- p
  xs <- many ( d >> p )
  return ( x : xs )
            ) <|> return []

skip :: Parser a b -> Parser a c -> Parser a b
skip rule ignore = rule >>= \ v -> ignore >> return v

(<<) :: Parser a b -> Parser a c -> Parser a b
(<<) = skip

infixl 3 !

(!) :: Parser a b -> Parser a b -> Parser a b
(!) = option
