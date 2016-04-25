module Youtan.Regex.DFMTest where

import Test.Hspec

import Control.Monad ( forM_ )

import Youtan.Regex.DFM
import Youtan.Regex.NDFM as NDFM ( fromString )

import Youtan.Regex.FMTestCases

spec :: SpecWith ()
spec = do
  context "matchDFM . fromNDFM" $
    forM_ cases $ \ ( regex, input, result, name ) ->
      it name $ matchDFM ( fromNDFM ( NDFM.fromString regex ) ) input `shouldBe` result

  context "matchDFM . group . fromNDFM" $
    forM_ cases $ \ ( regex, input, result, name ) ->
      it name $ matchDFM ( group ( fromNDFM ( NDFM.fromString regex ) ) ) input `shouldBe` result

  context "matchDFM . minimize . fromNDFM" $
    forM_ cases $ \ ( regex, input, result, name ) ->
      it name $ matchDFM ( minimize ( fromNDFM ( NDFM.fromString regex ) ) ) input `shouldBe` result

  context "match" $
    forM_ cases $ \ ( regex, input, result, name ) ->
      it name $ match regex input `shouldBe` result
