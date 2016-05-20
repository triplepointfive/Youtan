module Atom 
( ClassName ( .. )
, MethodName ( .. )
, PropertyName ( .. )
, VariableName ( .. )
) where

import Data.String ( IsString( .. ) )

newtype ClassName = ClassName String
  deriving ( Eq, Ord )

instance IsString ClassName where
  fromString = ClassName

instance Show ClassName where
  show ( ClassName name ) = name

newtype MethodName = MethodName String
  deriving ( Eq )

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
  deriving ( Eq )

instance IsString VariableName where
  fromString = VariableName

instance Show VariableName where
  show ( VariableName name ) = name
