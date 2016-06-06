module Main where

import           Control.Monad.State
import           Data.Char ( chr, ord )
import qualified Data.Sequence as Seq

import Youtan.Lexical.Tokenizer

data Token
  = IncrP
  | DecrP
  | IncrB
  | DecrB
  | Output
  | Input
  | Begin
  | End
  deriving ( Eq )

instance Show Token where
  show IncrP  = ">"
  show DecrP  = "<"
  show IncrB  = "+"
  show DecrB  = "-"
  show Output = "."
  show Input  = ","
  show Begin  = "["
  show End    = "]"

rules :: Rules Token
rules =
  [ ( ">", const IncrP )
  , ( "<", const DecrP )
  , ( "\\+", const IncrB )
  , ( "-", const DecrB )
  , ( "\\.", const Output )
  , ( ",", const Input )
  , ( "\\[", const Begin )
  , ( "\\]", const End )
  ]

drops :: [ String ]
drops = [ "[^-\\[\\]\\.\\,><\\+]+" ]

lexical :: String -> [ Token ]
lexical = tokenizeDrops rules drops

type Cell = Int

type Memory = ( Seq.Seq Int, Int, Seq.Seq Int )

type BF r = StateT Memory IO r

newMemory :: Memory
newMemory = ( Seq.empty, 0, Seq.empty )

eval :: Token -> BF ()
eval IncrB = modify ( second succ )
eval DecrB = modify ( second pred )
eval IncrP = do
  ( p, v, n ) <- get
  case Seq.viewl n of
    x Seq.:< xs -> put ( p Seq.|> v, x, xs )
    Seq.EmptyL  -> put ( p Seq.|> v, 0, n )
eval DecrP = do
  ( p, v, n ) <- get
  case Seq.viewr p of
    xs Seq.:> x -> put ( xs, x, v Seq.<| n )
    Seq.EmptyR  -> put (  p, 0, v Seq.<| n )
eval Output = do
  ( _, v, _ ) <- get
  lift ( putChar ( chr v ) )
eval Input = do
  v <- lift getChar
  modify ( second ( const ( ord v ) ) )

main :: IO ()
main = return ()

first :: ( a -> a ) -> ( a, b, c ) -> ( a, b, c )
first f ( a, b, c ) = ( f a, b, c )

second :: ( b -> b ) -> ( a, b, c ) -> ( a, b, c )
second f ( a, b, c ) = ( a, f b, c )

third :: ( c -> c ) -> ( a, b, c ) -> ( a, b, c )
third f ( a, b, c ) = ( a, b, f c )
