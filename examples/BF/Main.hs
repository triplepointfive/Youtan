module Main where

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
  | Rest
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
  show Rest   = ""

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
  , ( "[^-\\[\\]\\.\\,><\\+]+", const Rest )
  ]

lexical :: String -> [ Token ]
lexical = tokenize rules

main :: IO ()
main = return ()
