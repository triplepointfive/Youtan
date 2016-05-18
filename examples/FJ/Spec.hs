 {-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Lexical ( lexical )
import Syntax

main :: IO ()
main = hspec $ parallel $ describe "FJ" $ do
  describe "Syntax" $ do
    context "Expression" $ do
      it "Variable" $
        with expression ( lexical "fst" ) `shouldBe` Right ( Variable ( VariableName "fst" ) )
      it "Coercion" $
        with expression ( lexical "( Pair ) fst" ) `shouldBe` Right ( Coercion ( ClassName "Pair" ) ( Variable ( VariableName "fst" ) ) )
      it "Empty object" $
        with expression ( lexical "new A()" ) `shouldBe` Right ( Object ( ClassName "A" ) [] )
