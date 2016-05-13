{-# LANGUAGE OverloadedStrings #-}
module Youtan.Regex.DFMTest where

import Test.Hspec

import Control.Monad ( forM_ )
import Data.Monoid ( (<>) )

import Youtan.Regex.DFM
import Youtan.Regex.NDFM as NDFM ( fromString )

import Youtan.Regex.FMTestCases

spec :: SpecWith ()
spec = do
  context "matchFM . fromNDFM" $
    forM_ cases $ \ ( regex, input, result, name ) ->
      it name $ matchFM ( fromNDFM ( NDFM.fromString regex ) ) input `shouldBe` result

  context "matchFM . squeeze . fromNDFM" $
    forM_ cases $ \ ( regex, input, result, name ) ->
      it name $ matchFM ( squeeze ( fromNDFM ( NDFM.fromString regex ) ) ) input `shouldBe` result

  context "matchFM . minimize . fromNDFM" $
    forM_ cases $ \ ( regex, input, result, name ) ->
      it name $ matchFM ( minimize ( fromNDFM ( NDFM.fromString regex ) ) ) input `shouldBe` result

  context "match" $
    forM_ cases $ \ ( regex, input, result, name ) ->
      it name $ match regex input `shouldBe` result

  context "longestMatch" $
    forM_ longestCases $ \ ( regex, input, result, name ) ->
      it name $ ( fst <$> longestMatch regex input ) `shouldBe` result

  context "mappend" $ do
    context "Plain string" $ do
      let dfm = "new" <> "return" :: DFM

      it "First option matches" $
        matchFM dfm "new" `shouldBe` True

      it "Second option matches" $
        matchFM dfm "return" `shouldBe` True

      it "No match" $
        matchFM dfm "class" `shouldBe` False

    context "With different matchers" $ do
      let dfm = mconcat [ "a\\d*", "\\d+", "\\w*" ] :: DFM

      it "First option matches" $
        matchFM dfm "a12" `shouldBe` True

      it "Second option matches" $
        matchFM dfm "1123" `shouldBe` True

      it "Third option matches" $
        matchFM dfm "somestr" `shouldBe` True

      it "No match" $
        matchFM dfm "some lit" `shouldBe` False
