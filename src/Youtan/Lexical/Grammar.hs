{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Youtan.Lexical.Grammar where

-- import Control.Arrow ( first, second )
import Control.Monad.State
import Data.List ( inits )
-- import Data.Monoid ( (<>) )
import Data.Maybe ( fromMaybe )
-- import qualified Control.Monad.State as S ( State )

import Data.String

import Youtan.Regex.DFM ( match )

data Rule
  = Raw String
  -- | Any [ Rule ]
  -- | List [ Rule ]

-- anyOf :: [ Rule ] -> Rule
-- anyOf = List

instance Show Rule where
  show ( Raw x ) = x
  -- show ( List xs ) = concatMap show xs
  -- show ( Any xs ) = intercalate "|" $ map show xs

instance IsString Rule where
  fromString = Raw

-- instance Monoid Rule where
--   mempty = List []
--
--   mappend ( List xs ) ( List ys ) = List ( xs ++ ys )
--   mappend ( List [] ) x = x
--   mappend x ( List [] ) = x
--   mappend ( List xs ) x = List ( x : xs )
--   mappend x ( List xs ) = List ( x : xs )
--   mappend x y = List [ x, y ]

type Position = Int

data Oper = Plus | Minus
  deriving ( Show, Eq )

type P a = State Parser a

type Action = Parser -> Parser

data Parser = Parser Position String
--
-- class Parsable a where
--   parser :: P a
--
-- instance Parsable Oper where
--   parser = do
--     x <- parse "[-+]"
--     return $ if x == "+" then Plus else Minus
--
-- instance Parsable Integer where
--   parser = read <$> parse "-?\\d+"
--
--
--
new :: String -> Parser
new = Parser 0
--
-- -- parseWith :: Rule -> ( String -> a ) -> ( String -> a ) -> State Parser a
-- -- parseWith ( Raw regex ) f s = do
--
--

-- setAction :: Action -> P ()
-- setAction g = do
  -- Parser p inp _ <- get
  -- put ( Parser p inp g )
  --
move :: Position -> Parser -> Parser
move l ( Parser p inp ) = Parser ( p + l ) inp

parse' :: Rule -> P ( Maybe String )
parse' ( Raw regex ) = do
  Parser p inp <- get
  let x = filter ( match regex ) ( inits ( drop p inp ) )
      y = last x
      l = length y
  if null x
  then return Nothing
  else modify ( move l ) >> return ( Just y )

parse :: Rule -> P String
parse r@( Raw regex ) = fromMaybe ( error $ "No matches for " ++ show regex ++ "." ) <$> parse' r

choice :: [ ( Rule, String -> a ) ] -> P a
choice [] = error "All options failed"
choice ( ( r, f ) : xs ) = do
  x <- parse' r
  case x of
    Just s -> return ( f s )
    Nothing -> choice xs

lexem :: Rule -> P String
lexem = pad "\\s*" . parse

openBrace = lexem "\\("
closeBrace = lexem "\\)"

num :: P Integer
num = read <$> parse "-?\\d+"

-- exp = choice [
    -- ( padd openBrace ( parse "-?\\d+" ) closeBrace, read ),
    -- ( padd "\\s*" num, read ) ]

oper :: P Oper
oper = choice [ ( "\\+", const Plus ), ( "-", const Minus ) ]

pad :: Rule -> P a -> P a
pad p rule = do
  void ( parse p )
  x <- rule
  void ( parse p )
  return x

padd :: P a -> P b -> P c -> P b
padd f g h = f >> g >>= \ x -> h >> return x

-- -- expr :: P Integer
-- -- expr = pad "\\s*" parser >>= expr'
--
-- -- expr' :: Integer -> P Integer
-- -- expr' n = do
--   -- o <- parseM "[-+]"
--   -- case o of
--     -- Nothing -> return n
--     -- Just _ -> do
--       -- o <- parser
--       -- m <- pad "\\s*" parser
--       -- expr' ( if Plus == o then n + m else n - m )
--
--


string :: String -> R String
string = R

data R a = R String
         | F a

instance Functor R where
  fmap _ ( R s ) = R s
  fmap f ( F a ) = F ( f a )

instance Applicative R where
  pure = F
  (<*>) ( F f ) ( F a ) = F ( f a )
  -- (<*>) ( F f ) ( R s ) = F ( f a )

instance Monad R where

r :: R Int
r = return 0

p :: R Int
p = return 3

f :: R Int
f = do
  r
  x <- p
  r
  return x

runR :: R a -> String -> a
runR f inp = case f of
           F x -> x
           R p -> evalState ( parse ( Raw p ) ) ( new inp )


