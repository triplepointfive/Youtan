{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeChecker where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad.State

import AST
import Atom
import I18n

type Context = Map.Map VariableName ClassName

type Checker a = State ( Seq.Seq Error ) a

typeCheck :: NameTable -> Seq.Seq Error
typeCheck nameTable = evalState ( checkNameTable >> get ) Seq.empty
  where
    checkNameTable :: Checker ()
    checkNameTable = mapM_ checkClass ( Map.toList nameTable )

    isSubtype :: ClassName -> ClassName -> Bool
    isSubtype child parent
      | parent   == child = True
      | "Object" == child = False
      | otherwise = isSubtype ( parentClassName $ nameTable Map.! child ) parent

    -- TODO: Convert those to Map.
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

    checkClass :: ( ClassName, Class ) -> Checker ()
    checkClass ( className, Class{..} )
      = mapM_ checkMethod ( Map.toList classMethods )
      where
        thisContext = Map.singleton "this" className

        checkMethod :: ( MethodName, Method ) -> Checker ()
        checkMethod ( mName, Method{..} ) =
          check mBody `just` \ returnType ->
            unless ( returnType `isSubtype` retType ) $
              addError ( InvalidMethodReturnType mName retType ) returnType
          where
            context :: Context
            context = thisContext `Map.union` args

            check :: Expression -> Checker ( Maybe ClassName )
            check ( Variable name ) = nothing ( Map.lookup name context )
               ( addError VariableHasNoType name )
            check ( AttributeAccess expr attrName ) =
              check expr `onJust` \ subType ->
                ( attrName `Map.lookup` fields subType ) `nothing`
                  addError ( AccessingUknownAttr subType ) attrName
            check ( MethodInvocation expr name invocArgs ) = do
              method <- check expr `onJust` \ subType ->
                ( name `Map.lookup` methods subType ) `nothing`
                  addError ( UndefinedMethod subType ) name
              argsTypes <- allJust <$> mapM check invocArgs

              onJust ( return $ (,) <$> method <*> argsTypes ) $
                \ ( Method{..}, argsTypes ) ->
                  let gotCount = length argsTypes
                      expectedCount = Map.size args in do
                  if gotCount /= expectedCount
                  then
                    addError ( InvalidNumberOfArgs name expectedCount ) gotCount
                  else
                    forM_ ( zip argsTypes ( Map.toList args ) )
                      $ \ ( inputType, ( argName, argType ) ) ->
                        unless ( inputType `isSubtype` argType ) $
                          addError ( MethodArgsInvalidType
                            name argName argType ) inputType
                  return ( Just retType )

allJust :: [ Maybe a ] -> Maybe [ a ]
allJust = allJustStep []
  where
    -- TODO: Optimize.
    allJustStep acc [] = Just acc
    allJustStep acc ( Just v : xs ) = allJustStep ( acc ++ [ v ] ) xs
    allJustStep _   ( Nothing : _ ) = Nothing

onJust :: Monad m => m ( Maybe a ) -> ( a -> m ( Maybe b ) ) -> m ( Maybe b )
onJust extr f = do
  val <- extr
  case val of
    Just a  -> f a
    Nothing -> return Nothing

just :: Monad m => m ( Maybe a ) -> ( a -> m () ) -> m ()
just v f = void ( onJust v ( f >> return v ) )

nothing :: Monad m => Maybe a -> m b -> m ( Maybe a )
nothing Nothing f = f >> return Nothing
nothing v       _ = return v

addError :: ( a -> TypeCheckError ) -> a -> Checker ()
addError constr arg
  = modify ( Seq.|> Error "" 0 0 Fatal ( TypeCheckError ( constr arg ) ) )
