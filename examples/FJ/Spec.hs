{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Lexical ( Token, lexical )
import Syntax

parse :: Parser Token a -> String -> a
parse rule input = let Right res = with rule ( lexical input ) in res

main :: IO ()
main = hspec $ parallel $ describe "FJ" $ do
  describe "Syntax" $ do
    context "Expression" $ do
      it "Variable" $
        parse expression "fst" `shouldBe` 
          Variable "fst"
      it "This is also variable" $
        parse expression "this" `shouldBe` 
          Variable "this"
      it "Access variable attribute" $
        parse expression "some.prop" `shouldBe` 
          AttributeAccess ( Variable "some" ) "prop"
      it "Access variable attribute accessor" $
        parse expression "this.pair.fst" `shouldBe` 
          AttributeAccess ( AttributeAccess ( Variable "this" ) "pair" ) "fst"
      it "Attribute accessor of method invocation" $
        parse expression "this.pair().fst" `shouldBe` 
          AttributeAccess ( MethodInvocation ( Variable "this" ) "pair" [] ) "fst"
      it "Method invocation with no args" $
        parse expression "this.get()" `shouldBe` 
          MethodInvocation ( Variable "this" ) "get" []
      it "Method invocation with few args" $
        parse expression "this.get( a, b )" `shouldBe` 
          MethodInvocation ( Variable "this" ) "get" [ Variable "a", Variable "b" ]
      it "Method invocation of attribute access" $
        parse expression "this.first.get()" `shouldBe` 
          MethodInvocation ( AttributeAccess ( Variable "this" ) "first" ) "get" []
      it "Coercion" $
        parse expression "( Pair ) fst" `shouldBe` 
          Coercion "Pair" ( Variable "fst" )
      it "Empty object" $
        parse expression "new A()" `shouldBe` 
          Object "A" []
      it "Object with few args" $
        parse expression "new A(this, this.fst)" `shouldBe` 
          Object "A" [ Variable "this", AttributeAccess ( Variable "this" ) "fst" ]

    context "Class term" $ do
      it "Property" $
        parse classTerm "Object fst;" `shouldBe` 
          Property "Object" "fst"
      it "Minimal constructor" $
        parse classTerm "A() { super(); }" `shouldBe` 
          Constructor "A" [] ( ConstructorBody [] [] )
      it "Minimal method" $
        parse classTerm "A prop() { return this; }" `shouldBe` 
          Method "A" "prop" [] ( Variable "this" )

    context "Class" $ do
      it "Minimal class" $ do
        parse classDef "class A extends Object { A() { super(); } }" `shouldBe` 
          Class ( ClassHead "A" "Object" ) [ Constructor "A" [] ( ConstructorBody [] [] ) ]
