module Lexical
( Token( .. )
, lexical
) where

import Youtan.Lexical.Tokenizer ( Rules, tokenizeDrops )

data Token
  = OpenBrace
  | CloseBrace
  | OpenParentheses
  | CloseParentheses
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
  , ( "(new|class|extends|super|this|return)", Keyword )
  , ( "[^ \n\t(){};=,.]+", Identifier )
  ]

drops :: [ String ]
drops = [ "\\s+", "\\/\\/[^\n]*" ]

lexical :: String -> [ Token ]
lexical = tokenizeDrops rules drops
