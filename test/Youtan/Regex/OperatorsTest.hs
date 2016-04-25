module Youtan.Regex.OperatorsTest where

import Test.Hspec

import Control.Exception ( evaluate )

import Youtan.Regex.Operators

a, b, c :: Operator
a = Literal i 'a'
b = Literal i 'b'
c = Literal i 'c'

i :: OperatorID
i = initID

spec :: SpecWith ()
spec = context "parseString" $ do
  context "Base" $ do
    it "Empty string" $
      parseString "" `shouldBe` Empty i

    it "Plain string" $
      parseString "abc" `shouldBe` Concatenation i ( Concatenation i a b ) c

    it "Chars escaping" $
      parseString ".\\." `shouldBe` Concatenation i ( CharClass i Dot ) ( Literal i '.' )

  context "Counters" $ do
    it "Any" $
      parseString "a*" `shouldBe` Counts i KleeneStar a

    it "At least one" $
      parseString "a+" `shouldBe` Counts i OneOrMore a

    it "Optional" $
      parseString "a?" `shouldBe` Counts i ZeroOrOne a

    it "On group" $
      parseString "(a|b)*" `shouldBe` Counts i KleeneStar ( Group ( Disjunction i a b ) )

    it "In group" $
      parseString "(ab?)" `shouldBe` Group ( Concatenation i a ( Counts i ZeroOrOne b ) )

    it "Counter in disjunction" $
      parseString "a|b+" `shouldBe` Disjunction i a ( Counts i OneOrMore b )

  context "Disjunction" $ do
    it "Plain" $
      parseString "a|b|c" `shouldBe` Disjunction i a ( Disjunction i b c )

    it "Allows empty" $
      parseString "a|" `shouldBe` Disjunction i a ( Empty i )

    it "Grouping" $
      parseString "ab|bc|ca" `shouldBe` Disjunction i ( Concatenation i a b )
        ( Disjunction i ( Concatenation i b c ) ( Concatenation i c a ) )

  context "Char class" $ do
    it "Word" $
      parseString "\\w" `shouldBe` CharClass i Word

    it "Digit" $
      parseString "\\d" `shouldBe` CharClass i Digit

    it "Space" $
      parseString "\\s" `shouldBe` CharClass i Whitespace

    it "Not a word" $
      parseString "\\W" `shouldBe` CharClass i ( None Word )

    it "Not a digit" $
      parseString "\\D" `shouldBe` CharClass i ( None Digit )

    it "Not a space" $
      parseString "\\S" `shouldBe` CharClass i ( None Whitespace )

  context "Char groups" $ do
    it "Plain" $
      parseString "[abc]" `shouldBe` CharClass i ( CharGroup "cba" )

    it "Not in a list" $
      parseString "[^abc]" `shouldBe` CharClass i ( None ( CharGroup "cba" ) )

    it "List letters" $
      parseString "[a-e]" `shouldBe` CharClass i ( CharGroup "abcde" )

    it "List digits" $
      parseString "[0-9]" `shouldBe` CharClass i ( CharGroup "0123456789" )

    it "Escape" $
      parseString "[\\]]" `shouldBe` CharClass i ( CharGroup "]" )

  context "Repeater applied to repeater" $ do
    it "Optional on optional" $
      parseString "(a?)?" `shouldBe` Group ( Counts i ZeroOrOne a )

    it "At least one on at least one" $
      parseString "(a+)+" `shouldBe` Group ( Counts i OneOrMore a )

    it "Any with optional" $
      parseString "(a*)?" `shouldBe` Group ( Counts i KleeneStar a )

    it "Any with at least one" $
      parseString "(a+)*" `shouldBe` Group ( Counts i KleeneStar a )

    it "Optional on at least one" $
      parseString "(a+)?" `shouldBe` Group ( Counts i KleeneStar a )

  context "Fails" $ do
    it "Counter with no term" $
      evaluate ( parseString "+" ) `shouldThrow` anyException

    it "Group token is unclosed" $
      evaluate ( parseString "(ab" ) `shouldThrow` anyException

    it "Unclosed char-class group" $
      evaluate ( parseString "[ab" ) `shouldThrow` anyException

    it "Empty char-class group" $
      evaluate ( parseString "[]" ) `shouldThrow` anyException

    it "Incomplete escape sequence" $
      evaluate ( parseString "\\" ) `shouldThrow` anyException

    it "Unexpected close group token" $
      evaluate ( parseString "ab)" ) `shouldThrow` anyException
