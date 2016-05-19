module Atom 
( ClassName ( .. )
, MethodName ( .. )
, PropertyName ( .. )
, VariableName ( .. )
) where

import Data.String ( IsString( .. ) )

newtype ClassName = ClassName String
  deriving ( Show, Eq )

instance IsString ClassName where
  fromString = ClassName

newtype MethodName = MethodName String
  deriving ( Show, Eq )

instance IsString MethodName where
  fromString = MethodName

newtype PropertyName = PropertyName String
  deriving ( Show, Eq, Ord )

instance IsString PropertyName where
  fromString = PropertyName

newtype VariableName = VariableName String
  deriving ( Show, Eq )

instance IsString VariableName where
  fromString = VariableName
