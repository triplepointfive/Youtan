{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Sequence as Seq

import Control.Monad.State
import Test.Hspec

import Atom
import I18n
import Lexical ( lexical )
import Semantic
import Syntax
import TypeChecker ( typeCheck )

parse :: Parser Token a -> String -> a
parse rule input = let Right res = with rule ( lexical input ) in res

emptyClass :: ClassName -> ClassName -> ClassDef
emptyClass name parName
  = ClassDef ( ClassHead name parName ) [ ConstructorDef name [] ( ConstructorBody [] [] ) ]

errorMessages :: Semantic a -> Seq.Seq ErrorMessage
errorMessages f = evalState ( f >> fmap errorMessage <$> get ) Seq.empty

declareErrors :: Classes -> Seq.Seq ErrorMessage
declareErrors = errorMessages . declareClasses

classWithProps :: [ ( ClassName, PropertyName ) ] -> ClassDef
classWithProps props =
  ClassDef ( ClassHead "A" "Object" )
  (  ConstructorDef "A" [] ( ConstructorBody [] [] ) : map ( uncurry Property ) props )

classWithTerms :: [ ClassTerm ] -> ClassDef
classWithTerms = ClassDef ( ClassHead "A" "Object" )

main :: IO ()
main = hspec $ parallel $ describe "FJ" $ do
  describe "Syntax" $ do
    context "Expression" $ do
      it "Variable" $
        parse expression "fst" `shouldBe`
          Variable "fst"
      it "This is also variable" $
        parse expression "this" `shouldBe`
          Variable "this"
      it "Access variable attribute" $
        parse expression "some.prop" `shouldBe`
          AttributeAccess ( Variable "some" ) "prop"
      it "Access variable attribute accessor" $
        parse expression "this.pair.fst" `shouldBe`
          AttributeAccess ( AttributeAccess ( Variable "this" ) "pair" ) "fst"
      it "Attribute accessor of method invocation" $
        parse expression "this.pair().fst" `shouldBe`
          AttributeAccess ( MethodInvocation ( Variable "this" ) "pair" [] ) "fst"
      it "Method invocation with no args" $
        parse expression "this.get()" `shouldBe`
          MethodInvocation ( Variable "this" ) "get" []
      it "Method invocation with few args" $
        parse expression "this.get( a, b )" `shouldBe`
          MethodInvocation ( Variable "this" ) "get" [ Variable "a", Variable "b" ]
      it "Method invocation of attribute access" $
        parse expression "this.first.get()" `shouldBe`
          MethodInvocation ( AttributeAccess ( Variable "this" ) "first" ) "get" []
      it "Coercion" $
        parse expression "( Pair ) fst" `shouldBe`
          Coercion "Pair" ( Variable "fst" )
      it "Empty object" $
        parse expression "new A()" `shouldBe`
          Object "A" []
      it "Object with few args" $
        parse expression "new A(this, this.fst)" `shouldBe`
          Object "A" [ Variable "this", AttributeAccess ( Variable "this" ) "fst" ]

    context "Class term" $ do
      it "Property" $
        parse classTerm "Object fst;" `shouldBe`
          Property "Object" "fst"
      it "Minimal constructor" $
        parse classTerm "A() { super(); }" `shouldBe`
          ConstructorDef "A" [] ( ConstructorBody [] [] )
      it "Minimal method" $
        parse classTerm "A prop() { return this; }" `shouldBe`
          MethodTerm "A" "prop" [] ( Variable "this" )

    context "Class" $ do
      it "Minimal class" $ do
        parse classDef "class A extends Object { A() { super(); } }" `shouldBe`
          ClassDef ( ClassHead "A" "Object" ) [ ConstructorDef "A" [] ( ConstructorBody [] [] ) ]

  describe "Semantic" $ do
    context "Class declaration errors" $ do
      it "Class name is taken" $ do
        declareErrors [ emptyClass "A" "Object", emptyClass "A" "Object" ] `shouldBe`
          Seq.singleton ( SemanticError ( ClassIsDefined "A" ) )
      it "Class inherits unknown class" $ do
        declareErrors [ emptyClass "A" "B" ] `shouldBe`
          Seq.singleton ( SemanticError ( MissingParentClass "B" ) )
      it "Class inherits itself" $ do
        declareErrors [ emptyClass "A" "A" ] `shouldBe`
          Seq.singleton ( SemanticError ( MissingParentClass "A" ) )
      it "Does not complain for poorly defined parent class" $ do
        declareErrors [ emptyClass "A" "A", emptyClass "B" "A" ] `shouldBe`
          Seq.singleton ( SemanticError ( MissingParentClass "A" ) )
    context "Properties list errors" $ do
      it "Propery refers class itself" $ do
        declareErrors [ classWithProps [ ( "A", "a" ) ] ] `shouldBe`
          Seq.singleton ( SemanticError ( IncompleteClass "A" ) )
      it "Propery uses unknown class" $ do
        declareErrors [ classWithProps [ ( "B", "a" ) ] ] `shouldBe`
          Seq.singleton ( SemanticError ( UndefinedClass "B" ) )
      it "Propery is declared twice" $ do
        declareErrors [ classWithProps [ ( "Object", "a" ), ( "Object", "a" ) ] ] `shouldBe`
          Seq.singleton ( SemanticError ( DuplicatedPropertyName "a" ) )
    context "Constructor errors" $ do
      it "Classs without constructor" $ do
        declareErrors [ classWithTerms [] ] `shouldBe`
          Seq.singleton ( SemanticError ( MissingConstructor "A" ) )
      it "Classs with few constructors" $ do
        declareErrors [ classWithTerms [ ConstructorDef "A" [] ( ConstructorBody [] [] )
                                       , ConstructorDef "A" [] ( ConstructorBody [] [] ) ] ] `shouldBe`
          Seq.singleton ( SemanticError ( MultipleConstructorDeclarations "A" ) )
      it "Constructor returns another type" $ do
        declareErrors [ classWithTerms [ ConstructorDef "Object" [] ( ConstructorBody [] [] ) ] ] `shouldBe`
          Seq.singleton ( SemanticError ( ConstructorInvalidName "Object" ) )

  describe "Type checker" $ do
    context "Variable" $ do
      let check expr = errorMessage <$> ( typeCheck table )
            where
              ( Right table ) = semantic ( syntax ( lexical aClass ) )
              aClass = "class A extends Object { A() { super(); } " ++ expr ++ " }"
      it "Variable from params" $
        check "A a( A b ) { return b; }" `shouldBe` Seq.empty
      it "This is defined" $
        check "A a() { return this; }" `shouldBe` Seq.empty
      it "Undefined variable" $
        check "A a( A b ) { return c; }" `shouldBe`
          Seq.singleton ( TypeCheckError ( VariableHasNoType "c" ) )

    context "Attribute access" $ do
      let check expr = errorMessage <$> ( typeCheck table )
            where
              ( Right table ) = semantic ( syntax ( lexical aClass ) )
              aClass = "class A extends Object { Object fst; A() { super(); } " ++ expr ++ " }"
      it "Accesses self attribute" $
        check "Object a( A b ) { return this.fst; }" `shouldBe` Seq.empty
      it "Accesses input variable's attribute" $
        check "Object a( A b ) { return b.fst; }" `shouldBe` Seq.empty
      it "Ignores if internal term is undefined" $
        check "A a( A b ) { return c.fst; }" `shouldBe`
          Seq.singleton ( TypeCheckError ( VariableHasNoType "c" ) )
      it "Variable has no that property" $
        check "A a( A b ) { return this.snd; }" `shouldBe`
          Seq.singleton ( TypeCheckError ( AccessingUknownAttr "A" "snd" ) )
