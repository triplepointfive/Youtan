module I18n where

import Atom

data SemanticError
  = ClassIsDefined !ClassName
  | MissingParentClass !ClassName
  | UndefinedClass !ClassName
  | IncompleteClass !ClassName
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
