{-# LANGUAGE OverloadedStrings #-}
module Syntax
( ClassDef ( .. )
, ClassHead ( .. )
, ClassTerm ( .. )
, ConstructorBody ( .. )
, Expression ( .. )

, Classes
, Parser
, Token

, classDef
, classTerm
, expression
, syntax
, with
) where

import Youtan.Syntax.Parser

import Atom hiding ( MethodArguments )
import AST ( Expression( .. ) )
import Lexical ( Token( .. ) )

type MethodArguments = [ ( ClassName, VariableName ) ]

type Syntax = Parser Token

type Classes = [ ClassDef ]

syntax :: [ Token ] -> Either [ ( Classes, [ Token ] ) ] Classes
syntax = runParser grammar

with :: Parser a b -> [ a ] -> Either [ ( b, [ a ] ) ] b
with = runParser

data ClassDef = ClassDef !ClassHead ![ ClassTerm ]
  deriving ( Show, Eq )

data ClassHead
  = ClassHead
    { newClassName :: !ClassName
    , parentName   :: !ClassName
    }
  deriving ( Show, Eq )

data ClassTerm
  = ConstructorDef
    { buildType       :: !ClassName
    , constArgs       :: ![ ( ClassName, PropertyName ) ]
    , constructorBody :: !ConstructorBody
    }
  | Property !ClassName !PropertyName
  | MethodTerm
    { returnType :: !ClassName
    , methodName :: !MethodName
    , arguments  :: !MethodArguments
    , body       :: !Expression
    }
  deriving ( Show, Eq )

data ConstructorBody
  = ConstructorBody
    { superProperties :: ![ PropertyName ]
    , selfProperties  :: ![ ( PropertyName, PropertyName ) ]
    }
  deriving ( Show, Eq )

isIdentifier :: Token -> Bool
isIdentifier ( Identifier _ ) = True
isIdentifier _ = False

-- Meta rules.

openBrace, closeBrace, semicolon, dot :: Syntax ()
openParentheses, closeParentheses, equal, comma :: Syntax ()
openBrace        = term OpenBrace
closeBrace       = term CloseBrace
openParentheses  = term OpenParentheses
closeParentheses = term CloseParentheses
equal            = term Equal
semicolon        = term Semicolon
dot              = term Dot
comma            = term Comma

parenthesesed :: Syntax a -> Syntax a
parenthesesed p = pad openParentheses p closeParentheses

braced :: Syntax a -> Syntax a
braced p = pad openBrace p closeBrace

listOf :: Syntax a -> Syntax [ a ]
listOf = parenthesesed . joins comma

-- Keywords.

keyword :: String -> Syntax ()
keyword = term . Keyword

newKeyword, classKeyword, extendsKeyword :: Syntax ()
superKeyword, thisKeyword, returnKeyword :: Syntax ()
newKeyword     = keyword "new"
classKeyword   = keyword "class"
extendsKeyword = keyword "extends"
superKeyword   = keyword "super"
thisKeyword    = keyword "this"
returnKeyword  = keyword "return"

-- Strings.

identifier :: Syntax String
identifier = _identifier <$> satisfy isIdentifier

className :: Syntax ClassName
className = fromString <$> identifier

methodName' :: Syntax MethodName
methodName' = fromString <$> identifier

propertyName :: Syntax PropertyName
propertyName = fromString <$> identifier

variableName :: Syntax VariableName
variableName = fromString <$> identifier

-- Top level.

grammar :: Syntax [ ClassDef ]
grammar = some classDef

-- Class level.

classDef :: Syntax ClassDef
classDef = ClassDef
  <$> classHead
  <*> braced ( many classTerm )

classHead :: Syntax ClassHead
classHead = ClassHead
  <$> ( classKeyword   >> className )
  <*> ( extendsKeyword >> className )

classTerm :: Syntax ClassTerm
classTerm = property <|> constructor <|> method

property :: Syntax ClassTerm
property = Property <$> className <*> ( propertyName << semicolon )

-- Constructor level.

constructor :: Syntax ClassTerm
constructor = ConstructorDef
  <$> className
  <*> listOf constructorArg
  <*> braced constructorBody'

constructorBody' :: Syntax ConstructorBody
constructorBody' = ConstructorBody
  <$> ( superKeyword >> listOf propertyName << semicolon )
  <*> many selfAssignment

selfAssignment :: Syntax ( PropertyName, PropertyName )
selfAssignment = (,)
  <$> ( thisKeyword >> dot >> propertyName )
  <*> ( equal >> propertyName << semicolon )

-- Method level.

method :: Syntax ClassTerm
method = MethodTerm
  <$> className
  <*> methodName'
  <*> listOf methodArg
  <*> braced ( returnKeyword >> expression << semicolon )

methodArg :: Syntax ( ClassName, VariableName )
methodArg = (,) <$> className <*> ( variableName )

constructorArg :: Syntax ( ClassName, PropertyName )
constructorArg = (,) <$> className <*> ( propertyName )

-- Expression level.

expression :: Syntax Expression
expression = ( object <|> coercion <|> variable' ) >>= accessor

coercion :: Syntax Expression
coercion = Coercion
  <$> parenthesesed className
  <*> expression

variable' :: Syntax Expression
variable' = Variable <$> ( variableName ! return "this" << thisKeyword )

accessor :: Expression -> Syntax Expression
accessor expr = ( dot >> identifier >>= invocation expr >>= accessor )
              ! return expr

invocation :: Expression -> String -> Syntax Expression
invocation expr name
  = MethodInvocation expr ( MethodName name ) <$> listOf expression
  ! return ( AttributeAccess expr ( PropertyName name ) )

object :: Syntax Expression
object = Object
  <$> ( newKeyword >> className )
  <*> listOf expression
