module FJ.Parser where

import Data.List ( intercalate )

import Youtan.Lexical.Tokenizer
import Youtan.Syntax.Parser

import qualified Data.Map as Map

data Token
  = OpenBrace
  | CloseBrace
  | OpenParentheses
  | CloseParentheses
  | Space
  | Equal
  | Semicolon
  | Dot
  | Comma
  | Keyword { _keyword :: !String }
  | Identifier { _identifier :: !String }
  deriving ( Show, Eq )

rules :: Rules Token
rules =
  [ ( "\\{",  const OpenBrace )
  , ( "\\}",  const CloseBrace )
  , ( "\\(",  const OpenParentheses )
  , ( "\\)",  const CloseParentheses )
  , ( ";",    const Semicolon )
  , ( "=",    const Equal )
  , ( "\\.",  const Dot )
  , ( ",",    const Comma )
  , ( "\\s+", const Space )
  , ( "(new|class|extends|super|this|return)", Keyword )
  , ( "[^ \n\t(){};=]+", Identifier )
  ]
