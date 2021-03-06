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

    findMethod :: MethodName -> ClassName -> Checker ( Maybe Method )
    findMethod name subType = nothing
      ( name `Map.lookup` methods subType )
      ( addError ( UndefinedMethod subType ) name )

    findClass :: ClassName -> Checker ( Maybe Class )
    findClass name = nothing
      ( name `Map.lookup` nameTable )
      ( addError UnknownType name )

    checkClass :: ( ClassName, Class ) -> Checker ()
    checkClass ( className, curClass )
      = mapM_ checkMethod ( Map.toList ( classMethods curClass ) )
      where
        thisContext = Map.singleton "this" className

        checkMethod :: ( MethodName, Method ) -> Checker ()
        checkMethod ( mName, method ) = do
          void $ onJust
            ( checkWithContext context ( mBody method ) )
            $ \ returnType -> do
              unless ( returnType `isSubtype` retType method ) $
                addError ( InvalidMethodReturnType mName ( retType method ) ) returnType
              return Nothing
          void $ onJust
            ( return ( parentMethod ( parentClassName curClass ) ) )
            $ \ parMethod -> do
              unless 
                ( retType parMethod == retType method && 
                  argsOrder parMethod == argsOrder method ) 
                ( addError ( InvalidSignature className ) mName )
              return Nothing

          -- TODO Validate overrides.
          where
            context :: Context
            context = thisContext `Map.union` args method

            parentMethod :: ClassName -> Maybe Method
            parentMethod "Object" = Nothing
            parentMethod parClassName = mplus
              ( mName `Map.lookup` classMethods parClass )
              ( parentMethod ( parentClassName parClass ) )
              where
                parClass = nameTable Map.! parClassName

    checkWithContext :: Context -> Expression -> Checker ( Maybe ClassName )
    checkWithContext context = check
      where
        check :: Expression -> Checker ( Maybe ClassName )
        check ( Variable name ) = nothing ( Map.lookup name context )
           ( addError VariableHasNoType name )
        check ( AttributeAccess expr attrName ) =
          check expr `onJust` \ subType ->
            ( attrName `Map.lookup` fields subType ) `nothing`
              addError ( AccessingUknownAttr subType ) attrName
        check ( MethodInvocation expr name invocArgs ) =
          lift2M
            ( check expr `onJust` findMethod name )
            ( allJust <$> mapM check invocArgs )
            validSubexprs
          where
            validSubexprs :: Method -> [ ClassName ] -> Checker ClassName
            validSubexprs foundMethod argsTypes = do
              if gotCount == expectedCount
              then
                forM_ ( zip argsTypes ( Map.toList ( args foundMethod ) ) )
                  $ \ ( inputType, ( argName, argType ) ) ->
                    unless ( inputType `isSubtype` argType ) $
                      addError ( MethodArgsInvalidType
                        name argName argType ) inputType
              else
                addError ( InvalidNumberOfArgs name expectedCount ) gotCount
              return ( retType foundMethod )
              where
                gotCount = length argsTypes
                expectedCount = Map.size ( args foundMethod )
        check ( Object constrType args ) = do
          foundClass <- findClass constrType
          void $ lift2M
            ( return foundClass )
            ( allJust <$> mapM check args )
            validSubexprs
          return ( const constrType <$> foundClass )
          where
            validSubexprs :: Class -> [ ClassName ] -> Checker ()
            validSubexprs ( Class _ _ _ Constructor{..} ) argsTypes =
              if gotCount == expectedCount
              then
                forM_ ( zip argsTypes ( Map.toList constructorArgs ) )
                  $ \ ( inputType, ( argName, argType ) ) ->
                    unless ( inputType `isSubtype` argType ) $
                      addError ( ConstructorArgsInvalidType
                        constrType argName argType ) inputType
              else
                addError
                  ( ConstructorInvalidNumberOfArgs constrType expectedCount )
                  gotCount
              where
                gotCount = length argsTypes
                expectedCount = Map.size constructorArgs
        check ( Coercion castType subExpr ) = do
          foundClass <- findClass castType
          void $ lift2M
            ( return foundClass )
            ( check subExpr )
            validSubexprs
          return ( const castType <$> foundClass )
          where
            validSubexprs _ castFromType =
              unless ( castFromType `isSubtype` castType )
                ( addError ( InvalidTypeCasting castFromType ) castType )

lift2M :: Monad m
       => m ( Maybe a )
       -> m ( Maybe b )
       -> ( a -> b -> m c )
       -> m ( Maybe c )
lift2M a b f = do
  x1 <- a
  x2 <- b
  onJust ( return $ (,) <$> x1 <*> x2 ) ( fmap Just . uncurry f )

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

nothing :: Monad m => Maybe a -> m b -> m ( Maybe a )
nothing Nothing f = f >> return Nothing
nothing v       _ = return v

addError :: ( a -> TypeCheckError ) -> a -> Checker ()
addError constr arg
  = modify ( Seq.|> Error "" 0 0 Fatal ( TypeCheckError ( constr arg ) ) )
