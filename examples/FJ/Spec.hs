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
      it "Minimal class" $
        parse classDef "class A extends Object { A() { super(); } }" `shouldBe`
          ClassDef ( ClassHead "A" "Object" ) [ ConstructorDef "A" [] ( ConstructorBody [] [] ) ]

      it "Ignores commends" $ do
        let fileContent = unlines
              [ "// New class."
              , "class A extends Object {"
              , "  // Constructor"
              , "  A() {"
              , "    super();"
              , "  }"
              , "}"
              ]
        parse classDef fileContent `shouldBe`
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
      it "Constructor returns another type" $
        declareErrors [ classWithTerms [ ConstructorDef "Object" [] ( ConstructorBody [] [] ) ] ] `shouldBe`
          Seq.singleton ( SemanticError ( ConstructorInvalidName "Object" ) )
    context "Method errors" $
      it "Method has same name as class" $
        declareErrors [ classWithTerms [ ConstructorDef "A" [] ( ConstructorBody [] [] )
                                       , MethodTerm "Object" "A" [] ( Object "Object" [] ) ] ] `shouldBe`
          Seq.singleton ( SemanticError ( MethodIsConstructor "A" ) )

  describe "Type checker" $ do
    context "Method itself" $ do
      let check expr = errorMessage <$> typeCheck table
            where
              ( Right table ) = semantic ( syntax ( lexical aClass ) )
              aClass = "class A extends Object { Object fst; A() { super(); } " ++ expr ++ " }"
      it "Invalid return type" $
        check "A a( Object x2 ) { return x2; }" `shouldBe`
          Seq.singleton ( TypeCheckError ( InvalidMethodReturnType "a" "A" "Object" ) )

    context "Variable" $ do
      let check expr = errorMessage <$> typeCheck table
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
      let check expr = errorMessage <$> typeCheck table
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

    context "Method invocation" $ do
      let check expr = errorMessage <$> typeCheck table
            where
              ( Right table ) = semantic ( syntax ( lexical aClass ) )
              aClass = "class A extends Object { Object fst; A() { super(); } " ++ expr ++ " }"
      it "Method with no args" $
        check "Object a( Object b ) { return this.a( b ); }" `shouldBe` Seq.empty
      it "Method with single arg" $
        check "Object a( Object b ) { return this.a( b ); }" `shouldBe` Seq.empty
      it "Method lacks for an arg" $
        check "Object a( Object b ) { return this.a(); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( InvalidNumberOfArgs "a" 1 0 ) )
      it "Method has extra args" $
        check "Object a( Object b ) { return this.a( b, this ); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( InvalidNumberOfArgs "a" 1 2 ) )
      it "Validates args even if method could not be resolved" $
        check "Object a( ) { return some.a( b, this ); }" `shouldBe`
          Seq.fromList [ TypeCheckError ( VariableHasNoType "some" ), TypeCheckError ( VariableHasNoType "b" ) ]
      it "Invalid type of argument" $
        check "Object a( A x1, Object x2 ) { return x1.a( x2, x2 ); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( MethodArgsInvalidType "a" "x1" "A" "Object" ) )
      it "Method is not defined for class" $
        check "Object a( ) { return this.b(); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( UndefinedMethod "A" "b" ) )

    context "New object" $ do
      let check expr = errorMessage <$> typeCheck table
            where
              ( Right table ) = semantic ( syntax ( lexical ( bClass ++ cClass ++ aClass ) ) )
              bClass = "class B extends Object { Object fst; B( Object fst ) { super(); this.fst = fst; } }"
              cClass = "class C extends Object { B fst; C( B fst ) { super(); this.fst = fst; } }"
              aClass = "class A extends Object { Object fst; A( Object fst ) { super(); this.fst = fst; } " ++ expr ++ " }"
      it "New with no args" $
        check "Object a( Object b ) { return new Object(); }" `shouldBe` Seq.empty
      it "New with single arg" $
        check "A a( Object b ) { return new A(b); }" `shouldBe` Seq.empty
      it "New lacks for an arg" $
        check "A a( Object b ) { return new A(); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( ConstructorInvalidNumberOfArgs "A" 1 0 ) )
      it "New has extra args" $
        check "Object a( Object b ) { return new Object( b ); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( ConstructorInvalidNumberOfArgs "Object" 0 1 ) )
      it "Validates args even if method could not be resolved" $
        check "Object a() { return new D( b ); }" `shouldBe`
          Seq.fromList [ TypeCheckError ( UnknownType "D" ), TypeCheckError ( VariableHasNoType "b" ) ]
      it "Invalid type of argument" $
        check "C a( Object x ) { return new C( x ); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( ConstructorArgsInvalidType "C" "fst" "B" "Object" ) )

    context "Type coercion" $ do
      let check expr = errorMessage <$> typeCheck table
            where
              ( Right table ) = semantic ( syntax ( lexical ( bClass ++ cClass ++ aClass ) ) )
              bClass = "class B extends Object { B() { super(); } }"
              cClass = "class C extends B { C() { super(); } }"
              aClass = "class A extends Object { A() { super(); } " ++ expr ++ " }"
      it "Type is exactly the same" $
        check "C a() { return ( C ) new C(); }" `shouldBe` Seq.empty
      it "Cast to parent class" $
        check "B a() { return ( B ) new C(); }" `shouldBe` Seq.empty
      it "Cast to Object via another class" $
        check "Object a( Object b ) { return ( Object ) new C(); }" `shouldBe` Seq.empty
      it "Cannot cast parent to a child" $
        check "C a() { return ( C ) new B(); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( InvalidTypeCasting "B" "C" ) )
      it "Cannot cast to unrelated class" $
        check "A a() { return ( A ) new B(); }" `shouldBe`
          Seq.singleton ( TypeCheckError ( InvalidTypeCasting "B" "A" ) )
      it "Returns type even if expression is invalid" $
        check "B a() { return ( B ) b; }" `shouldBe`
          Seq.singleton ( TypeCheckError ( VariableHasNoType "b" ) )
      it "Complains for unknown class" $
        check "B a() { return ( D ) b; }" `shouldBe`
          Seq.fromList [ TypeCheckError ( UnknownType "D" ), TypeCheckError ( VariableHasNoType "b" ) ]

