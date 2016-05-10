module Parser.JSONTest where

import Prelude hiding ( null )

import qualified Data.Map as Map
import Test.Hspec

import Parser.JSON

fullExample, strippedExample  :: String
fullExample = unlines
  [ "{"
  , "  \"id\": 1,"
  , "  \"name\": \"A green door\","
  , "  \"price\": 12.50,"
  , "  \"tags\": [\"home\", \"green\"]"
  , "}"
  ]
strippedExample = concat
  [ "{"
  , "\"id\":1.0,"
  , "\"name\":\"A green door\","
  , "\"price\":12.5,"
  , "\"tags\":[\"home\",\"green\"]"
  , "}"
  ]

spec :: SpecWith ()
spec = do
  context "lexem" $ do
    it "Ignores leading spaces" $
      with ( lexem null ) " null" `shouldBe` Right JSNull
    it "Ignores trailing spaces" $
      with ( lexem null ) "null " `shouldBe` Right JSNull
  context "plain" $ do
    it "null" $
      with null "null" `shouldBe` Right JSNull
    it "true" $
      with true "true" `shouldBe` Right ( JSBool True )
    it "false" $
      with false "false" `shouldBe` Right ( JSBool False )
  context "array" $ do
    it "Empty" $
      with array "[]" `shouldBe` Right ( JSArray [] )
    it "Nested" $
      with array "[[  ]]" `shouldBe` Right ( JSArray [ JSArray [] ] )
    it "Few elements" $
      with array "[1,2]" `shouldBe` Right ( JSArray [ JSRational 1, JSRational 2 ] )
    it "Heterogeneous" $
      with array "[false, null ]" `shouldBe` Right ( JSArray [ JSBool False, JSNull ] )
  context "object" $ do
    it "Empty" $
      with object "{}" `shouldBe` Right ( JSObject Map.empty )
    it "Few elements" $
      with object "{\"a\":1,\"b\" :2}" `shouldBe` Right ( JSObject ( Map.fromList
        [ ( "a", JSRational 1 )
        , ( "b", JSRational 2 )
        ]  ) )
    it "Heterogeneous" $
      with object "{ \"a\" : {}, \"flag\":null}" `shouldBe` Right ( JSObject ( Map.fromList
        [ ( "a", JSObject Map.empty )
        , ( "flag", JSNull )
        ]  ) )
    it "All types" $
      with object fullExample `shouldBe` Right ( JSObject ( Map.fromList
        [ ( "id", JSRational 1 )
        , ( "name", JSString "A green door" )
        , ( "price", JSRational 12.50 )
        , ( "tags", JSArray [ JSString "home", JSString "green" ] )
        ]  ) )
  it "show" $
    ( show <$> with object fullExample ) `shouldBe` Right strippedExample
