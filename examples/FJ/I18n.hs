module I18n where

import Atom

data SemanticError
  = ClassIsDefined !ClassName
  | MissingParentClass !ClassName
  | UndefinedClass !ClassName

  | IncompleteClass !ClassName
  | DuplicatedPropertyName !PropertyName

  | MissingConstructor !ClassName
  | ConstructorInvalidName !ClassName
  | MultipleConstructorDeclarations !ClassName

  | PropertyIsNotSet !PropertyName
  | UnknownProperty !PropertyName
  | ParentPropertySet !PropertyName

  | UndefinedVariable !VariableName
  | MethodIsDefined !MethodName
  | VariableIsDefined !VariableName
  deriving Eq

instance Show SemanticError where
  show ( ClassIsDefined name ) =
    concat [ "class ", show name, " has already been defined" ]
  show ( MissingParentClass name ) =
    concat [ "class ", show name, " is not defined" ]
  show ( UndefinedClass name ) =
    concat [ "class ", show name, " is not defined" ]

  show ( IncompleteClass name ) =
    concat [ "class ", show name, " is incomplete" ]
  show ( DuplicatedPropertyName name ) =
    concat [ "property ", show name, " has already been defined" ]

  show ( MissingConstructor name ) =
    concat [ "class ", show name, " has no defined constructor" ]
  show ( ConstructorInvalidName name ) =
    concat [ "constructor ", show name, " must match a class name" ]
  show ( MultipleConstructorDeclarations name ) =
    concat [ "constructor for ", show name, " has already been defined" ]

  show ( PropertyIsNotSet name ) =
    concat [ "property ", show name, " is not set" ]
  show ( UnknownProperty name ) =
    concat [ "unknown property ", show name, " is set" ]
  show ( ParentPropertySet name ) =
    concat [ "property ", show name, " belongs to parent class and must be set via super call" ]
  show ( UndefinedVariable name ) =
    "undefined variable " ++ show name
  show ( MethodIsDefined name ) =
    concat [ "method ", show name, " has already been defined" ]
  show ( VariableIsDefined name ) =
    concat [ "variable ", show name, " has already been defined" ]

data TypeCheckError
  = VariableHasNoType !VariableName
  | AccessingUknownAttr !ClassName !PropertyName
  | InvalidMethodReturnType !MethodName !ClassName !ClassName
  | UndefinedMethod !ClassName !MethodName
  | InvalidNumberOfArgs !MethodName !Int !Int
  | MethodArgsInvalidType !MethodName !VariableName !ClassName !ClassName
  deriving Eq

instance Show TypeCheckError where
  show ( VariableHasNoType name ) =
    "could not resolve variable " ++ show name
  show ( AccessingUknownAttr className name ) =
    concat [ "class ", show className, " does not have property ", show name ]
  show ( InvalidMethodReturnType name expected got ) =
    concat [ "invalid return type of method ", show name,
      ": expected ", show expected, " got ", show got ]
  show ( UndefinedMethod className name ) =
    concat [ "class ", show className, " does not have a method ", show name ]
  show ( InvalidNumberOfArgs name expected got ) =
    concat [ "invalid number of arguments for method ", show name,
      ": expected ", show expected, ", got ", show got ]
  show ( MethodArgsInvalidType name argName expected got ) =
    concat [ "method ", show name, " got invalid type for argument ", show argName,
      ": expected ", show expected, ", got ", show got ]

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

type Errors = [ Error ]

data ErrorMessage
  = SemanticError !SemanticError
  | TypeCheckError !TypeCheckError
  deriving Eq

instance Show ErrorMessage where
  show ( SemanticError  message ) = show message
  show ( TypeCheckError message ) = show message
