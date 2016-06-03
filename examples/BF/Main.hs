module Main where

import           Control.Arrow
import           Control.Monad.State
import           Data.Char ( chr )
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
  , ( ".", const Output )
  , ( ",", const Input )
  , ( "\\[", const Begin )
  , ( "\\]", const End )
  ]

drops :: [ String ]
drops = [ "[^-\\[\\]\\.\\,><\\+]+" ]

lexical :: String -> [ Token ]
lexical = tokenizeDrops rules drops

type Cell = Int

type Memory = ( Int, Seq.Seq Int )

type BF r = StateT Memory IO r

eval :: Token -> BF ()
eval IncrP = do
  modify ( first succ )
  ( v, s ) <- get
  when ( v >= Seq.length s ) $
    modify ( second ( Seq.|> 0 ) )
eval DecrP = do
  v <- fst <$> get
  if v == 0
    then
      modify ( second ( 0 Seq.<| ) )
    else
      modify ( first pred )

main :: IO ()
main = return ()
