module Youtan.Lexical.TokenizerTest where

import Test.Hspec

import Youtan.Lexical.Tokenizer

data Oper = Plus | Minus
  deriving ( Show, Eq )

data AlgExpression
  = Num Integer
  | Action Oper
  | Rest String
  deriving ( Eq, Show )

algRules :: Rules AlgExpression
algRules = [ ( "\\+", const ( Action Plus ) )
           , ( "-", const ( Action Minus ) )
           , ( "-?\\d+", Num . read )
           , ( "[^+-\\d]+", Rest )
           ]

spec :: SpecWith ()
spec = context "parseString" $ do
  context "AlgExpression" $ do
    it "Empty string" $
      tokenize algRules "" `shouldBe` []
    it "Few tokens" $
      tokenize algRules "12+34" `shouldBe` [ Num 12, Action Plus, Num 34 ]
    it "Collects unknown chars" $
      tokenize algRules "12abc+34" `shouldBe` [ Num 12, Rest "abc", Action Plus, Num 34 ]

