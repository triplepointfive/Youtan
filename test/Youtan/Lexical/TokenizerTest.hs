module Youtan.Lexical.TokenizerTest where

import Test.Hspec

import Control.Exception ( evaluate )

import Youtan.Lexical.Tokenizer

data Oper = Plus | Minus
  deriving ( Show, Eq )

data AlgExpression
  = Num Integer
  | Action Oper
  | Rest String
  deriving ( Eq, Show )

algRules :: Rules AlgExpression
algRules = [ ( "\\+", const ( Action Plus ) )
           , ( "-", const ( Action Minus ) )
           , ( "-?\\d+", Num . read )
           , ( "[^+-\\d]+", Rest )
           ]

data JavaToken
  = Comment String
  | Keyword String
  | Brace String
  | Dot
  | Colon
  | Space
  | Iden String
  | Str String
  deriving ( Show, Eq )

javaHelloWorld :: String
javaHelloWorld = unlines
  [ "/* HelloWorld.java"
  , "*/"
  , ""
  , "public class HelloWorld"
  , "{"
  , "\tpublic static void main(String[] args) {"
  , "\t\tSystem.out.println(\"Hello World!\");"
  , "\t}"
  , "}"
  ]

javaRules :: Rules JavaToken
javaRules =
  [ ( "\\/\\*.*\\*/", Comment )
  , ( "(public|class|static|void)", Keyword )
  , ( "[{}()\\[\\]]", Brace )
  , ( "\\.", const Dot )
  , ( ";", const Colon )
  , ( "\\\"[^\\\"]*\\\"", Str )
  , ( "\\s+", const Space )
  , ( "[a-zA-Z][0-9a-zA-Z]*", Iden )
  ]

dropSpaces :: [ JavaToken ] -> [ JavaToken ]
dropSpaces [] = []
dropSpaces ( Space : xs ) = dropSpaces xs
dropSpaces ( x : xs ) = x : dropSpaces xs

javaHelloWorldTokens :: [ JavaToken ]
javaHelloWorldTokens =
  [ Comment "/* HelloWorld.java\n*/"
  , Keyword "public", Keyword "class", Iden "HelloWorld"
  , Brace "{", Keyword "public", Keyword "static", Keyword "void", Iden "main", Brace "(", Iden "String", Brace "[", Brace "]",Iden "args",Brace ")",Brace "{"
  , Iden "System",Dot,Iden "out",Dot,Iden "println",Brace "(",Str "\"Hello World!\"",Brace ")",Colon
  , Brace "}"
  , Brace "}"
  ]

spec :: SpecWith ()
spec = context "parseString" $ do
  context "tokenize" $ do
    it "Empty string" $
      tokenize algRules "" `shouldBe` []
    it "Few tokens" $
      tokenize algRules "12+34" `shouldBe` [ Num 12, Action Plus, Num 34 ]
    it "Collects unknown chars" $
      tokenize algRules "12abc+34" `shouldBe` [ Num 12, Rest "abc", Action Plus, Num 34 ]
    it "Fails with no parse" $
      evaluate ( tokenize [ ( "-", id ) ] "+" :: [ String ] ) `shouldThrow` anyException
    it "Ignores wildchars" $
      tokenize [ ( "a*", id ), ( "-", id ) ] "-" `shouldBe` [ "-" ]
    it "" $
      dropSpaces ( tokenize javaRules javaHelloWorld ) `shouldBe` javaHelloWorldTokens

  context "tokenizeT" $ do
    it "Empty string" $
      tokenizeT algRules "" `shouldBe` Right []
    it "Few tokens" $
      tokenizeT algRules "12+34" `shouldBe` Right [ Num 12, Action Plus, Num 34 ]
    it "Collects unknown chars" $
      tokenizeT algRules "12abc+34" `shouldBe` Right [ Num 12, Rest "abc", Action Plus, Num 34 ]
    it "Fails with no parse" $
      tokenizeT [ ( "-", id ) ] "+" `shouldBe`
        ( Left "All options failed +" :: Either String [ String ] )
    it "Ignores wildchars" $
      tokenizeT [ ( "a*", id ), ( "-", id ) ] "-" `shouldBe` Right [ "-" ]
