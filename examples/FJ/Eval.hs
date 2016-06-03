{-# LANGUAGE RecordWildCards #-}
module Eval where

import           Data.List ( intercalate )
import qualified Data.Map as Map

import           AST
import           Atom

data Value
  = Value
    { valType :: !ClassName
    , attrs   :: !(Map.Map PropertyName Value )
    } deriving Eq


instance Show Value where
  show Value{..} = concat
    [ "new "
    , show valType
    , "("
    , intercalate ", " ( map showArgs $ Map.toList attrs )
    , ")"
    ]
    where
      showArgs :: ( PropertyName, Value ) -> String
      showArgs ( prop, val ) = show prop ++ ": " ++ show val

type Context = Map.Map VariableName Value

eval :: Context -> NameTable -> Expression -> Value
eval context _ ( Variable name ) = context Map.! name
eval context table ( Object objType values ) = Value
  { valType = objType
  , attrs   = Map.fromList ( map prop ( Map.keys $ properties objType ) )
  }
  where
    evaledArgs :: [ Value ]
    evaledArgs = map ( eval context table ) values 

    inputVars :: Map.Map PropertyName Value
    inputVars = Map.fromList ( zip ( inputProps valConstructor ) evaledArgs )

    valClass = table Map.! objType
    valConstructor = constructor valClass

    -- TODO: Remove duplication.
    properties :: ClassName -> Properties
    properties ( ClassName "Object" ) = Map.empty
    properties name = let foundClass = table Map.! name
                       in classProperties foundClass `Map.union`
                          properties ( parentClassName foundClass )

    prop :: PropertyName -> ( PropertyName, Value )
    prop name 
      | name `Map.member` ( assigns valConstructor )
        = ( name, inputVars Map.! ( assigns valConstructor Map.! name ) )
