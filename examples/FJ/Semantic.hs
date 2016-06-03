{-# LANGUAGE OverloadedStrings #-}
module Semantic where

import qualified Data.Map as Map
import Data.Foldable ( toList )
import qualified Data.Sequence as Seq
import Control.Monad.State

import AST
import Atom
import I18n
import Syntax

type Semantic a = State ( Seq.Seq Error ) a

semantic :: Either [ ( Classes, [ Token ] ) ] Classes -> Either Errors NameTable
semantic = either parseError nameTable

parseError :: [ ( Classes, [ Token ] ) ] -> Either Errors NameTable
parseError = undefined

nameTable :: Classes -> Either Errors NameTable
nameTable classes = evalState ( declareClasses classes >>= anyOf ) Seq.empty

newNameTable :: NameTable
newNameTable = Map.singleton "Object" ( Class Map.empty "Object" Map.empty emptyConstr )

emptyConstr :: Constructor
emptyConstr = Constructor [] Map.empty [] Map.empty

declareClasses :: Classes -> Semantic NameTable
declareClasses = foldM addClass newNameTable

-- TODO: Check parent properties list.
addClass :: NameTable -> ClassDef -> Semantic NameTable
addClass classes ( ClassDef ( ClassHead name parClassName ) terms ) = do
  definedClass parClassName /-> addError' MissingParentClass parClassName
  definedClass name --> addError' ClassIsDefined name

  properties <- foldM validateProperties Map.empty
    [ ( className, propName ) | Property className propName     <- terms ]

  constr <- validateConstructor constructors

  validatedMethods <- foldM validateMethod Map.empty methods

  return ( Map.insert name ( Class properties parClassName validatedMethods constr ) classes )
  where
    constructors = [ ( retVal, a, b )
                   | ConstructorDef retVal a b <- terms ]
    methods      = [ ( retVal, metName, a, b )
                   | MethodTerm retVal metName a b <- terms ]

    definedClass = flip Map.member classes
    incompleteClass = (==) name

    -- TODO: Doesn't colliase with parent properties.
    validateProperties :: Properties
                       -> ( ClassName, PropertyName )
                       -> Semantic Properties
    validateProperties props ( className, propName ) = do
      incompleteClass className --> addError'  IncompleteClass className
      when ( className `Map.notMember` classes && className /= name ) $
        addError'  UndefinedClass className
      when ( propName `Map.member` props ) ( addError' DuplicatedPropertyName propName )
      return ( Map.insert propName className props )

    validateConstructor :: [ ( ClassName, [ ( ClassName, PropertyName ) ], ConstructorBody ) ]
                        -> Semantic Constructor
    validateConstructor [] = do
      addError' MissingConstructor name
      return emptyConstr
    validateConstructor [ ( className, argS, ConstructorBody super self ) ] = do
      when ( className /= name ) ( addError' ConstructorInvalidName className )

      props <- foldM validateProperties Map.empty argS

      return Constructor
        { inputProps      = map snd argS
        , constructorArgs = props
        , superArgs       = super
        , assigns         = Map.fromList self
        }
    validateConstructor ( c : _ ) = do
      addError' MultipleConstructorDeclarations name
      validateConstructor [ c ]

    validateMethod :: Methods
                   -> ( ClassName, MethodName, [ ( ClassName, VariableName ) ], Expression )
                   -> Semantic Methods
    validateMethod list ( retVal, mName, argS, expr ) = do
      when ( name `isSame` mName ) ( addError' MethodIsConstructor mName )
      when ( mName `Map.member` list ) ( addError' MethodIsDefined mName )
      when ( retVal `Map.notMember` classes && retVal /= name )
        ( addError' UndefinedClass retVal )

      validatedArgs <- foldM validateMethodArg Map.empty argS

      let method = Method
            { retType   = retVal
            , args      = validatedArgs
            , mBody     = expr
            , argsOrder = map fst argS
            }

      return ( Map.insert mName method list )

    validateMethodArg :: MethodArguments
                      -> ( ClassName, VariableName )
                      -> Semantic MethodArguments
    validateMethodArg list ( varType, varName ) = do
      when ( varName `Map.member` list ) ( addError' VariableIsDefined varName )

      when ( varType `Map.notMember` classes && varType /= name )
        ( addError' UndefinedClass varType )

      return ( Map.insert varName varType list )

anyOf :: NameTable -> Semantic ( Either Errors NameTable )
anyOf table = do
  declarationErrors <- get
  if Seq.null declarationErrors
  then
    return ( Right table )
  else
    return ( Left ( toList declarationErrors ) )

addError' :: ( a -> SemanticError ) -> a -> Semantic ()
addError' constr arg
  = modify ( Seq.|> Error "" 0 0 Fatal ( SemanticError ( constr arg ) ) )

infix 2 -->
(-->) :: Applicative f => Bool -> f () -> f ()
(-->) = when

infix 2 /->
(/->) :: Applicative f => Bool -> f () -> f ()
(/->) = unless
