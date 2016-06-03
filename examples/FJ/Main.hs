module Main where

import qualified Data.Map as Map

import AST
import Lexical ( lexical )
import Semantic ( semantic )
import Syntax ( syntax )
import TypeChecker ( typeCheck )
import Eval

main :: IO ()
main = return ()
