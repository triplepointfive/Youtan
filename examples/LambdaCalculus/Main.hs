module Main where

-- import Test.Hspec

import Youtan.Lexical.Tokenizer
import Youtan.Syntax.CFG

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

str :: Grammar Token String
str = FMap ( single iden ) _iden
  where
    iden ( Iden _ ) = True
    iden          _ = False

name :: Grammar Token Exp
name = FMap str RawName

funArgs :: Grammar Token [ String ]
funArgs = toList str

fun :: Grammar Token Exp
fun = ( \ ( ( ( _, n), _), e) -> RawAbs n e ) <$> tok Lambda :& funArgs :& tok Dot :& expression

app :: Grammar Token Exp
app = uncurry RawApp <$> expression :& expression

expression :: Grammar Token Exp
expression = brace expression 
           :| name 
           :| fun 
           :| app

assignment :: Grammar Token Assignment
assignment = ( \ ( ( n, _), e ) -> Assignment n e ) <$> str :& tok Equal :& expression

grammar :: Grammar Token [ Assignment ]
grammar = toList assignment

with :: String -> Maybe Exp
with input = parse ( dropSpaces $ tokenize rules input ) expression

brace :: Grammar Token a -> Grammar Token a
brace g = ( \ ( ( _, x ), _ ) -> x ) <$> tok OpenBrace :& g :& tok CloseBrace

-- spec :: SpecWith ()
-- spec =
  -- context "parse" $ do undefined
    -- it "Plain number" $
      -- with "12" `shouldBe` Just ( E 12 )
    -- it "Expression refers itself" $
      -- with "12+31" `shouldBe` Just ( A ( E 12 ) Plus ( E 31 ) )
    -- it "Few nested expressions" $
      -- with "1+2-3" `shouldBe` Just ( A ( A ( E 1 ) Plus ( E 2 ) ) Minus ( E 3 ) )

main :: IO ()
main = return ()
