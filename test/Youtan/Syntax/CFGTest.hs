module Youtan.Syntax.CFGTest where

import Test.Hspec

import Youtan.Lexical.Tokenizer
import Youtan.Syntax.CFG

expRules :: Rules ExpToken
expRules = [ ( "-", const ( Action Minus ) )
           , ( "\\+", const ( Action Plus ) )
           , ( "-?\\d+", Num . read )
           ]

data Oper = Plus | Minus
  deriving ( Show, Eq )

data ExpToken
  = Num Integer
  | Action Oper
  deriving ( Show )

instance Eq ExpToken where
  ( Num _ )    == ( Num _ ) = True
  ( Action _ ) == ( Action _ ) = True
  _ == _ = False

data Exp
  = E Integer
  | A Exp Oper Exp
  deriving ( Eq, Show )

n :: Grammar ExpToken Exp
n = term ( Num 0 ) (\ ( Num x )-> E x )

op :: Grammar ExpToken Oper
op = term ( Action Plus ) (\ ( Action o  ) -> o)

expression :: Grammar ExpToken Exp
expression = ( buildExp <$> expression :& op :& expression ) :| n

buildExp :: ((Exp, Oper), Exp) -> Exp
buildExp ( ( e1, o ), e2 ) = A e1 o e2

with :: String -> Maybe Exp
with input = parse ( tokenize expRules input ) expression

spec :: SpecWith ()
spec =
  context "parse" $ do
    it "Plain number" $
      with "12" `shouldBe` Just ( E 12 )
    it "Expression refers itself" $
      with "12+31" `shouldBe` Just ( A ( E 12 ) Plus ( E 31 ) )
    it "Few nested expressions" $
      with "1+2-3" `shouldBe` Just ( A ( A ( E 1 ) Plus ( E 2 ) ) Minus ( E 3 ) )
