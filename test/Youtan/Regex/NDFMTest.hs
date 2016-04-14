module Youtan.Regex.NDFMTest where

import Test.Hspec

import Control.Monad ( forM_ )

import Youtan.Regex.NDFM

import Youtan.Regex.FMTestCases

spec :: SpecWith ()
spec = context "match" $
  forM_ cases $ \ ( regex, input, result, name ) ->
    it name $ match regex input `shouldBe` result
