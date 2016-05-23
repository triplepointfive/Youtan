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
    { classProperties :: !( Set.Set PropertyName )
    , classMethods    :: !( Map.Map MethodName Method )
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

type ClassesDeclaration = Map.Map ClassName ( [ PropertyName ], [ MethodName ] )

newClassesDeclaration :: ClassesDeclaration
newClassesDeclaration = Map.singleton "Object" ( [], [] )

declareClasses :: Classes -> Semantic ClassesDeclaration
declareClasses = foldM addClass newClassesDeclaration

addClass :: ClassesDeclaration -> ClassDef -> Semantic ClassesDeclaration
addClass classes ( ClassDef ( ClassHead name parentName ) terms ) = do
  mapM_ addError classDefErrors  

  properties <- validateProperties

  -- validateConstructor terms
  
  return ( Map.insert name ( [], [] ) classes )
  where
    classDefErrors = map snd $ filter fst $
      [ ( parentName `Map.notMember` classes
        , MissingParentClass parentName )
      , ( name `Map.member` classes
        , ClassIsDefined name )
      ]

    validateProperties :: Semantic [ PropertyName ]
    validateProperties = forM properties $ \ ( className, propName ) -> do
      when ( className == name ) ( addError ( IncompleteClass className ) )
      when ( className `Map.notMember` classes && className /= name ) $
        addError ( IncompleteClass className )
      return propName
      where
        properties = [ ( className, propName ) | Property className propName <- terms ]


    -- validateConstructor :: ClassesDeclaration -> Semantic ClassesDeclaration
    -- validateConstructor terms

addError :: SemanticError -> Semantic ()
addError message 
  = modify ( Seq.|> Error "" 0 0 Fatal ( SemanticError message ) )

anyOf :: ( [ b ], a ) -> Either [ b ] a
anyOf ( [], a ) = Right a
anyOf ( xs, _ ) = Left xs
