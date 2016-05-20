module Semantic where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Sequence as S
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
    { file     :: !FilePath
    , column   :: !Int
    , line     :: !Int
    , severity :: !Severity
    , message  :: !ErrorMessage
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

type Semantic a = State ( Seq Error ) a

semantic :: Either [ ( Classes, [ Token ] ) ] Classes -> Either Errors NameTable
semantic = either parseError nameTable

parseError :: [ ( Classes, [ Token ] ) ] -> Either Errors NameTable
parseError = undefined

nameTable :: Classes -> Either Errors NameTable
nameTable = undefined -- declareClasses

type ClassesDeclaration = Map.Map ClassName ( [ PropertyName ], [ MethodName ] )

declareClasses :: Classes -> Semantic ClassesDeclaration
declareClasses = foldM addClass Map.empty
  where
    addClass :: ClassesDeclaration -> ClassDef -> Semantic ClassesDeclaration
    addClass classes ( ClassDef ( ClassHead name parentName ) terms ) = do
      unless ( parentName `Map.member` classes ) $ 
        addError ( MissingParentClass parentName )

      if name `Map.member` classes
      then do
        addError ( ClassIsDefined name )
        return classes
      else
        return ( Map.insert name ( [], [] ) classes )

addError :: SemanticError -> Semantic ()
addError message = modify ( |> Error "" 0 0 Fatal ( SemanticError message ) )

anyOf :: ( [ b ], a ) -> Either [ b ] a
anyOf ( [], a ) = Right a
anyOf ( xs, _ ) = Left xs
