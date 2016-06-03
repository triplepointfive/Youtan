module AST where

import qualified Data.Map as Map

import Atom

data Expression
  = Variable !VariableName
  | AttributeAccess !Expression !PropertyName
  | MethodInvocation !Expression !MethodName ![ Expression ]
  | Object !ClassName ![ Expression ]
  | Coercion !ClassName !Expression
  deriving ( Show, Ord, Eq )

type NameTable = Map.Map ClassName Class

type Methods = Map.Map MethodName Method

data Class
  = Class
    { classProperties :: !Properties
    -- TODO: The only usage is to extract parent data. Think of doing it once.
    , parentClassName :: !ClassName
    , classMethods    :: !Methods
    , constructor     :: !Constructor
    }
  deriving Show

data Constructor
  = Constructor
    -- | List of input args in the original order.
    { inputProps      :: ![ PropertyName ]
    -- | Input args with their types.
    , constructorArgs :: !Properties
    -- | List of variables, passed to super call.
    , superArgs       :: ![ PropertyName ]
    -- | Assignments in body.
    , assigns         :: !( Map.Map PropertyName PropertyName )
    }
  deriving Show

data Method
  = Method
    { retType   :: !ClassName
    , args      :: !MethodArguments
    , mBody     :: !Expression
    , argsOrder :: ![ ClassName ]
    }
  deriving Show
