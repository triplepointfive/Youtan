{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeChecker where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad.State

import AST
import Atom
import I18n

type Context = Map.Map Expression ClassName

type Checker a = State ( Seq.Seq Error ) a

-- typeCheck ::
typeCheck nameTable = undefined
  where
    currentClass = undefined

    thisContext = Map.singleton ( Variable "this" ) currentClass

    fields :: ClassName -> Properties
    fields ( ClassName "Object" ) = Map.empty
    fields name = let foundClass = nameTable Map.! name
                   in classProperties foundClass `Map.union`
                      fields ( parentClassName foundClass )

    methods :: ClassName -> Methods
    methods ( ClassName "Object" ) = Map.empty
    methods name = let foundClass = nameTable Map.! name
                    in classMethods foundClass `Map.union`
                       methods ( parentClassName foundClass )

    methodContext :: Method -> Context
    methodContext Method{..}
      = thisContext `Map.union`
        Map.mapKeys Variable args

    check context = undefined
      where
        ch :: Expression -> Checker ( Maybe ClassName )
        ch var@( Variable name ) = case Map.lookup var context of
          Just typeName -> return ( Just typeName )
          Nothing       -> do
            addError VariableHasNoType name
            return Nothing


addError :: ( a -> TypeCheckError ) -> a -> Checker ()
addError constr arg
  = modify ( Seq.|> Error "" 0 0 Fatal ( TypeCheckError ( constr arg ) ) )
