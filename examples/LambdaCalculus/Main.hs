module Main where

import Youtan.Lexical.Tokenizer
import Youtan.Syntax.CFG

data Token
  = Iden { _iden :: String }
  | Dot
  | Lambda
  | Space
  | OpenBrace
  | CloseBrace
  deriving ( Eq, Show )

rules :: Rules Token
rules =
  [ ( "\\\\", const Lambda )
  , ( "\\.", const Dot )
  , ( "\\(", const OpenBrace )
  , ( "\\)", const CloseBrace )
  , ( "\\s+", const Space )
  , ( "[^\\.() ]+", Iden )
  ]

dropSpaces :: [ Token ] -> [ Token ]
dropSpaces = filter ( /= Space )

data Exp
  = Name String
  | Abs String Exp
  | App Exp Exp
  deriving ( Show, Eq )

str :: Grammar Token String
str = FMap ( single iden ) _iden
  where
    iden ( Iden _ ) = True
    iden          _ = False

name :: Grammar Token Exp
name = FMap str Name

fun :: Grammar Token Exp
fun = ( \ ( ( ( _, n), _), e) -> Abs n e ) <$> tok Lambda :& str :& tok Dot :& expression

app :: Grammar Token Exp
app = ( uncurry App ) <$> expression :& expression

expression :: Grammar Token Exp
expression = brace expression :| name :| fun :| app

with :: String -> Maybe Exp
with input = parse ( dropSpaces $ tokenize rules input ) expression

brace :: Grammar Token a -> Grammar Token a
brace g = ( \ ( ( _, x ), _ ) -> x ) <$> tok OpenBrace :& g :& tok CloseBrace
