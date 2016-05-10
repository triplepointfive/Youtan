module Parser.JSON where

import Prelude hiding ( null )

import Data.List ( intercalate )

import Youtan.Lexical.Tokenizer
import Youtan.Syntax.Parser

import qualified Data.Map as Map

data JS
  = JSNull
  | JSBool !Bool
  | JSRational !Double
  | JSString !String
  | JSArray ![ JS ]
  | JSObject !( Map.Map String JS )
  deriving ( Eq )

instance Show JS where
  show JSNull = "null"
  show ( JSBool True )   = "true"
  show ( JSBool False )  = "false"
  show ( JSRational v )  = show v
  show ( JSString str )  = show str
  show ( JSArray arr )   = show arr
  show ( JSObject list ) = concat 
    [ "{"
    , intercalate "," ( map ( \ ( n, v ) -> show n ++ ":" ++ show v ) $ Map.toList list ) 
    , "}"
    ]

data Token
  = OpenBrace
  | CloseBrace
  | OpenSquareBrace
  | CloseSquareBrace
  | Colon
  | Comma
  | Space
  | Number { _num :: !Double }
  | RawString { _str :: !String }
  | Literal !String
  deriving ( Show, Eq )

rules :: Rules Token
rules =
  [ ( ":", const Colon )
  , ( ",", const Comma )
  , ( "\\{", const OpenBrace )
  , ( "\\}", const CloseBrace )
  , ( "\\[", const OpenSquareBrace )
  , ( "\\]", const CloseSquareBrace )
  , ( "-?(0|\\d+)(\\.\\d+)?", Number . read )
  , ( "\\\"[^\"]*\\\"", RawString . tail . init )
  , ( "\\s+", const Space )
  , ( "(true|false|null)", Literal )
  ]

isNum, isStr :: Token -> Bool
isNum s = case s of
  Number _ -> True
  _ -> False
isStr s = case s of
  RawString _ -> True
  _ -> False

with :: Parser Token a -> String -> Either [ ( a, [ Token ] ) ] a
with parser = runParser parser . tokenize rules 

grammar :: Parser Token JS
grammar = value

value :: Parser Token JS
value = lexem ( null <|> true <|> false <|> number <|> string <|> array <|> object )

null, true, false, number, string, array, object :: Parser Token JS
null   = const JSNull           <$> term ( Literal "null" )
true   = const ( JSBool True )  <$> term ( Literal "true" )
false  = const ( JSBool False ) <$> term ( Literal "false" )
number = ( JSRational . _num )  <$> satisfy isNum
string = ( JSString . _str )    <$> satisfy isStr
array  = JSArray <$> pad openSquareBrace ( joins comma value ) closeSquareBrace
object = JSObject . Map.fromList <$> pad openBrace ( joins comma objectPair ) closeBrace

lexem :: Parser Token a -> Parser Token a
lexem p = pad ( opt Space ) p ( opt Space )

comma, colon :: Parser Token ()
comma = lexem ( term Comma )
colon = lexem ( term Colon )

openSquareBrace, closeSquareBrace, openBrace, closeBrace :: Parser Token ()
openSquareBrace  = lexem ( term OpenSquareBrace )
closeSquareBrace = lexem ( term CloseSquareBrace )
openBrace        = lexem ( term OpenBrace )
closeBrace       = lexem ( term CloseBrace )

objectPair :: Parser Token ( String, JS )
objectPair = do
  name <- _str <$> lexem ( satisfy isStr )
  colon
  val <- value
  return ( name, val )
