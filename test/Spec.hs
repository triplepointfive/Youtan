import Test.Hspec
import Control.Exception (evaluate)

import Youtan.Regex.Operators
import Youtan.Regex.NDFM

a, b, c :: Operator
a = Literal 'a'
b = Literal 'b'
c = Literal 'c'

main :: IO ()
main = hspec $ do
  describe "Operators.parseString" $ do
    it "Empty string" $ do
      parseString "" `shouldBe` Empty

    it "Plain string" $ do
      parseString "abc" `shouldBe` Concatenation ( Concatenation a b ) c

    it "Options" $ do
      parseString "a|b|c" `shouldBe` Disjunction a ( Disjunction b c )

    it "Counter kleene star" $ do
      parseString "a*" `shouldBe` Counts a KleeneStar

    it "Counter plus" $ do
      parseString "a+" `shouldBe` Counts a OneOrMore

    it "Counter question mark" $ do
      parseString "a?" `shouldBe` Counts a ZeroOrOne

    it "Counter on group" $ do
      parseString "(a|b)*" `shouldBe` Counts ( Disjunction a b ) KleeneStar

    it "Disjunction grouping" $ do
      parseString "ab|bc|ca" `shouldBe` Disjunction ( Concatenation a b ) ( Disjunction ( Concatenation b c ) ( Concatenation c a ) )

  describe "NDFM.match" $ do
    it "Empty regex with empty string" $ do
      match "" "" `shouldBe` True

    it "Empty regex with none empty string" $ do
      match "" "abc" `shouldBe` False

    it "Single char regex with same char string" $ do
      match "a" "a" `shouldBe` True

    it "Single char regex with another char string" $ do
      match "a" "b" `shouldBe` False

    it "Plain regex with same plain string" $ do
      match "abc" "abc" `shouldBe` True

    it "Plain regex with different plain string" $ do
      match "abc" "acc" `shouldBe` False
