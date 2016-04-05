import Test.Hspec

import Youtan.Regex.Operators
import Youtan.Regex.NDFM

import Control.Exception ( evaluate )

a, b, c :: Operator
a = Literal 'a'
b = Literal 'b'
c = Literal 'c'

main :: IO ()
main = hspec $ do
  describe "Operators.parseString" $ do
    it "Empty string" $
      parseString "" `shouldBe` Empty

    it "Plain string" $
      parseString "abc" `shouldBe` Concatenation ( Concatenation a b ) c

    it "Options" $
      parseString "a|b|c" `shouldBe` Disjunction a ( Disjunction b c )

    it "Counter kleene star" $
      parseString "a*" `shouldBe` Counts KleeneStar a

    it "Counter plus" $
      parseString "a+" `shouldBe` Counts OneOrMore a

    it "Counter question mark" $
      parseString "a?" `shouldBe` Counts ZeroOrOne a

    it "Counter on group" $
      parseString "(a|b)*" `shouldBe` Counts KleeneStar ( Disjunction a b )

    it "Counter in group" $
      parseString "(ab?)" `shouldBe` Concatenation a ( Counts ZeroOrOne b )

    it "Counter in disjunction" $
      parseString "a|b+" `shouldBe` Disjunction a ( Counts OneOrMore b )

    it "Disjunction grouping" $
      parseString "ab|bc|ca" `shouldBe` Disjunction ( Concatenation a b ) ( Disjunction ( Concatenation b c ) ( Concatenation c a ) )

    it "Character classes" $
      parseString "\\w|\\d|\\s" `shouldBe` Disjunction ( CharClass Word ) ( Disjunction ( CharClass Digit ) ( CharClass Whitespace ) )

    it "Chars escaping" $
      parseString ".\\." `shouldBe` Concatenation ( CharClass Dot ) ( Literal '.' )

    it "Group token is unclosed" $
      evaluate ( parseString "(ab" ) `shouldThrow` anyException

  describe "NDFM.match" $ do
    it "Empty regex with empty string" $
      match "" "" `shouldBe` True

    it "Empty regex with none empty string" $
      match "" "abc" `shouldBe` False

    it "Single char regex with same char string" $
      match "a" "a" `shouldBe` True

    it "Single char regex with another char string" $
      match "a" "b" `shouldBe` False

    it "Plain regex with same plain string" $
      match "abc" "abc" `shouldBe` True

    it "Plain regex with different plain string" $
      match "abc" "acc" `shouldBe` False

    it "Literal disjunction matching branch 1" $
      match "a|b" "a" `shouldBe` True

    it "Literal disjunction matching branch 1" $
      match "a|b" "b" `shouldBe` True

    it "Literal disjunction matching nothing" $
      match "a|b" "c" `shouldBe` False

    it "Literal disjunction with few branches and matching" $
      match "a|b|c|d|e" "e" `shouldBe` True

    it "Literal disjunction with few branches without matching" $
      match "a|b|c|d|e" "t" `shouldBe` False
