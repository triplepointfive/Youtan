module Atom
( ClassName ( .. )
, MethodName ( .. )
, PropertyName ( .. )
, VariableName ( .. )
, Properties
, MethodArguments
, fromString
) where

import qualified Data.Map as Map
import Data.String ( IsString( .. ) )

newtype ClassName = ClassName String
  deriving ( Eq, Ord )

instance IsString ClassName where
  fromString = ClassName

instance Show ClassName where
  show ( ClassName name ) = name

newtype MethodName = MethodName String
  deriving ( Eq, Ord )

instance IsString MethodName where
  fromString = MethodName

instance Show MethodName where
  show ( MethodName name ) = name

newtype PropertyName = PropertyName String
  deriving ( Eq, Ord )

instance IsString PropertyName where
  fromString = PropertyName

instance Show PropertyName where
  show ( PropertyName name ) = name

newtype VariableName = VariableName String
  deriving ( Eq, Ord )

instance IsString VariableName where
  fromString = VariableName

instance Show VariableName where
  show ( VariableName name ) = name

type Properties = Map.Map PropertyName ClassName

type MethodArguments = Map.Map VariableName ClassName
