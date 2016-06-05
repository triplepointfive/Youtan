module Main where

import Control.Applicative ( many, some )

import Youtan.Lexical.Tokenizer
import Youtan.Syntax.Parser

data Token
  = Iden { _iden :: String }
  | Dot
  | Lambda
  | Space
  | Equal
  | OpenBrace
  | CloseBrace
  deriving ( Eq, Show )

rules :: Rules Token
rules =
  [ ( "\\\\", const Lambda )
  , ( "\\.", const Dot )
  , ( "=", const Equal )
  , ( "\\(", const OpenBrace )
  , ( "\\)", const CloseBrace )
  , ( "\\s+", const Space )
  , ( "[^\\.() \n]+", Iden )
  ]

dropSpaces :: [ Token ] -> [ Token ]
dropSpaces = filter ( /= Space )

data Exp
  = RawName String
  | RawAbs [ String ] Exp
  | RawApp Exp Exp
  deriving ( Show, Eq )

data Assignment = Assignment String Exp
  deriving ( Show, Eq )

with :: String -> Either [ ( [ Assignment ], [ Token ] ) ] [ Assignment ]
with input = runParser grammar ( dropSpaces $ tokenize rules input )

main :: IO ()
main = return ()

str :: Parser Token String
str = _iden <$> satisfy iden
  where
    iden ( Iden _ ) = True
    iden          _ = False

name :: Parser Token Exp
name = RawName <$> str

funArgs :: Parser Token [ String ]
funArgs = some str

fun :: Parser Token Exp
fun = do
  term Lambda
  args <- funArgs
  term Dot
  ex <- expression
  return $ RawAbs args ex

app :: Parser Token Exp
app = do
  ex <- expression
  ex2 <- expression
  return $ RawApp ex ex2

expression :: Parser Token Exp
expression = ( brace expression ) `option` name `option` fun `option` app

brace :: Parser Token a -> Parser Token a
brace g = do
  term OpenBrace
  x <- g
  term CloseBrace
  return x

assignment :: Parser Token Assignment
assignment = do
  a <- str
  term Equal
  e <- expression
  return $ Assignment a e

grammar :: Parser Token [ Assignment ]
grammar = many assignment
