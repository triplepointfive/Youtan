import Test.Hspec
import Control.Exception (evaluate)

import Youtan.Regex.Operators

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
