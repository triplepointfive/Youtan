{-# LANGUAGE OverloadedStrings #-}
module Semantic where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Control.Monad.State

import Atom
import I18n
import Syntax

type Errors = [ Error ]

data Severity
  = Fatal
  | Note
  deriving ( Show, Eq )

data Error
  = Error
    { file         :: !FilePath
    , column       :: !Int
    , line         :: !Int
    , severity     :: !Severity
    , errorMessage :: !ErrorMessage
    }
  deriving ( Show, Eq )

data ErrorMessage
  = SemanticError SemanticError
  deriving Eq

instance Show ErrorMessage where
  show ( SemanticError message ) = show message

type NameTable = Map.Map ClassName Class

data Class
  = Class
    { classProperties :: !Properties
    , classMethods    :: !( Map.Map MethodName Method )
    }
  deriving Show

data Constructor
  = Constructor
    { inputProps      :: ![ PropertyName ]
    , constructorArgs :: !Properties
    , superArgs       :: ![ PropertyName ]
    }
  deriving Show

data Method = Method
  deriving Show

type Semantic a = State ( Seq.Seq Error ) a

semantic :: Either [ ( Classes, [ Token ] ) ] Classes -> Either Errors NameTable
semantic = either parseError nameTable

parseError :: [ ( Classes, [ Token ] ) ] -> Either Errors NameTable
parseError = undefined

nameTable :: Classes -> Either Errors NameTable
nameTable = undefined -- declareClasses

type ClassesDeclaration = Map.Map ClassName ( Properties, [ MethodName ] )

newClassesDeclaration :: ClassesDeclaration
newClassesDeclaration = Map.singleton "Object" ( Map.empty, [] )

declareClasses :: Classes -> Semantic ClassesDeclaration
declareClasses = foldM addClass newClassesDeclaration

addClass :: ClassesDeclaration -> ClassDef -> Semantic ClassesDeclaration
addClass classes ( ClassDef ( ClassHead name parentName ) terms ) = do
  mapM_ addError classDefErrors

  properties <- foldM validateProperties Map.empty properties

  constructor <- validateConstructor constructors

  return ( Map.insert name ( properties, [] ) classes )
  where
    properties   = [ ( className, propName ) | Property className propName     <- terms ]
    constructors = [ ( retVal, args, body )  | ConstructorDef retVal args body <- terms ]

    classDefErrors = map snd $ filter fst $
      [ ( parentName `Map.notMember` classes
        , MissingParentClass parentName )
      , ( name `Map.member` classes
        , ClassIsDefined name )
      ]

    -- TODO: Doesn't colliase with parent properties.
    validateProperties :: Properties
                       -> ( ClassName, PropertyName )
                       -> Semantic Properties
    validateProperties props ( className, propName ) = do
      when ( className == name ) ( addError ( IncompleteClass className ) )
      when ( className `Map.notMember` classes && className /= name ) $
        addError ( UndefinedClass className )
      when ( propName `Map.member` props ) ( addError ( DuplicatedPropertyName propName ) )
      return ( Map.insert propName className props )

    validateConstructor :: [ ( ClassName, [ ( ClassName, PropertyName ) ], ConstructorBody ) ] 
                        -> Semantic ( Maybe Constructor )
    validateConstructor [] = do
      addError ( MissingConstructor name )
      return Nothing
    validateConstructor [ ( className, args, ConstructorBody super self ) ] = do
      when ( className /= name ) ( addError ( ConstructorInvalidName className ) )

      props <- foldM validateProperties Map.empty args

      -- TODO: Check parent properties list.



      -- let

      return ( Just $ Constructor [] props [])
    validateConstructor ( c : xs ) = do
      addError ( MultipleConstructorDeclarations name )
      validateConstructor [ c ]

addError :: SemanticError -> Semantic ()
addError message
  = modify ( Seq.|> Error "" 0 0 Fatal ( SemanticError message ) )

anyOf :: ( [ b ], a ) -> Either [ b ] a
anyOf ( [], a ) = Right a
anyOf ( xs, _ ) = Left xs
