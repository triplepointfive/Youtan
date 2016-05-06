module Youtan.Syntax.Parser where

newtype Parser a b = Parser { parse :: [ a ] -> [( b, [ a ] )] }

runParser :: Parser a b -> [ a ] -> b
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

-- instance Functor Parser where
  -- fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

-- instance Applicative Parser where
  -- pure = return
  -- (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

-- instance Monad Parser where
  -- return = unit
  -- (>>=)  = bind

item :: Parser a a
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

bind :: Parser a b -> ( b -> Parser a c ) -> Parser a c
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') ( parse p s )

unit :: b -> Parser a b
unit a = Parser (\s -> [(a,s)])

satisfy :: ( b -> Bool ) -> Parser a b
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else (Parser (const []))
