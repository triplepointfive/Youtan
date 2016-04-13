module Youtan.Regex.OperatorsTest where

import Test.Hspec

import Control.Exception ( evaluate )

import Youtan.Regex.Operators

a, b, c :: Operator
a = Literal 'a'
b = Literal 'b'
c = Literal 'c'

spec :: SpecWith ()
spec = context "parseString" $ do
  context "Base" $ do
    it "Empty string" $
      parseString "" `shouldBe` Empty

    it "Plain string" $
      parseString "abc" `shouldBe` Concatenation ( Concatenation a b ) c

    it "Chars escaping" $
      parseString ".\\." `shouldBe` Concatenation ( CharClass Dot ) ( Literal '.' )

  context "Counters" $ do
    it "Any" $
      parseString "a*" `shouldBe` Counts KleeneStar a

    it "At least one" $
      parseString "a+" `shouldBe` Counts OneOrMore a

    it "Optional" $
      parseString "a?" `shouldBe` Counts ZeroOrOne a

    it "On group" $
      parseString "(a|b)*" `shouldBe` Counts KleeneStar ( Group ( Disjunction a b ) )

    it "In group" $
      parseString "(ab?)" `shouldBe` Group ( Concatenation a ( Counts ZeroOrOne b ) )

    it "Counter in disjunction" $
      parseString "a|b+" `shouldBe` Disjunction a ( Counts OneOrMore b )

  context "Disjunction" $ do
    it "Plain" $
      parseString "a|b|c" `shouldBe` Disjunction a ( Disjunction b c )

    it "Allows empty" $
      parseString "a|" `shouldBe` Disjunction a Empty

    it "Grouping" $
      parseString "ab|bc|ca" `shouldBe` Disjunction ( Concatenation a b )
        ( Disjunction ( Concatenation b c ) ( Concatenation c a ) )

  context "Char class" $ do
    it "Word" $
      parseString "\\w" `shouldBe` CharClass Word

    it "Digit" $
      parseString "\\d" `shouldBe` CharClass Digit

    it "Space" $
      parseString "\\s" `shouldBe` CharClass Whitespace

    it "Not a word" $
      parseString "\\W" `shouldBe` CharClass ( None Word )

    it "Not a digit" $
      parseString "\\D" `shouldBe` CharClass ( None Digit )

    it "Not a space" $
      parseString "\\S" `shouldBe` CharClass ( None Whitespace )

  context "Char groups" $ do
    it "Plain" $
      parseString "[abc]" `shouldBe` CharClass ( CharGroup "cba" )

    it "Not in a list" $
      parseString "[^abc]" `shouldBe` CharClass ( None ( CharGroup "cba" ) )

    it "List letters" $
      parseString "[a-e]" `shouldBe` CharClass ( CharGroup "abcde" )

    it "List digits" $
      parseString "[0-9]" `shouldBe` CharClass ( CharGroup "0123456789" )

    it "Escape" $
      parseString "[\\]]" `shouldBe` CharClass ( CharGroup "]" )

  context "Repeater applied to repeater" $ do
    it "Optional on optional" $
      parseString "(a?)?" `shouldBe` Group ( Counts ZeroOrOne a )

    it "At least one on at least one" $
      parseString "(a+)+" `shouldBe` Group ( Counts OneOrMore a )

    it "Any with optional" $
      parseString "(a*)?" `shouldBe` Group ( Counts KleeneStar a )

    it "Any with at least one" $
      parseString "(a+)*" `shouldBe` Group ( Counts KleeneStar a )

    it "Optional on at least one" $
      parseString "(a+)?" `shouldBe` Group ( Counts KleeneStar a )

  context "Fails" $ do
    it "Counter with no term" $
      evaluate ( parseString "+" ) `shouldThrow` anyException

    it "Group token is unclosed" $
      evaluate ( parseString "(ab" ) `shouldThrow` anyException

    it "Group inside a group" $
      evaluate ( parseString "((a))" ) `shouldThrow` anyException

    it "Group in a disjunction" $
      evaluate ( parseString "(a|(b))" ) `shouldThrow` anyException

    it "Unclosed char-class group" $
      evaluate ( parseString "[ab" ) `shouldThrow` anyException

    it "Empty char-class group" $
      evaluate ( parseString "[]" ) `shouldThrow` anyException

    it "Incomplete escape sequence" $
      evaluate ( parseString "\\" ) `shouldThrow` anyException

    it "Unexpected close group token" $
      evaluate ( parseString "ab)" ) `shouldThrow` anyException

