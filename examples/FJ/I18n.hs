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
