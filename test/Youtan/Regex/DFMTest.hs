module Youtan.Regex.DFMTest where

import Test.Hspec

import Control.Monad ( forM_ )

import Youtan.Regex.DFM
import Youtan.Regex.NDFM as NDFM ( fromString )

import Youtan.Regex.FMTestCases

spec :: SpecWith ()
spec = context "match" $
  forM_ cases $ \ ( regex, input, result, name ) ->
    it name $ matchDFM ( fromNDFM ( NDFM.fromString regex )) input `shouldBe` result
