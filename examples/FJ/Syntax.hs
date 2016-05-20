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

import Atom
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
    { className  :: !ClassName
    , parentName :: !ClassName
    }
  deriving ( Show, Eq )

data ClassTerm
  = Constructor
    { buildType       :: !ClassName
    , args            :: !MethodArguments
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
    , selfProperties  :: ![ ( PropertyName, VariableName ) ]
    }
  deriving ( Show, Eq )

data Expression
  = Variable !VariableName
  | AttributeAccess !Expression !PropertyName
  | MethodInvocation !Expression !MethodName ![ Expression ]
  | Object !ClassName ![ Expression ]
  | Coercion !ClassName !Expression
  deriving ( Show, Eq )

isIdentifier :: Token -> Bool
isIdentifier ( Identifier _ ) = True
isIdentifier _ = False

-- Meta rules.

lexem :: Syntax a -> Syntax a
lexem p = pad ( opt Space ) p ( opt Space )

openBrace, closeBrace, semicolon, dot, space :: Syntax ()
openParentheses, closeParentheses, equal, comma :: Syntax ()
openBrace        = lexem $ term OpenBrace
closeBrace       = lexem $ term CloseBrace
openParentheses  = lexem $ term OpenParentheses
closeParentheses = lexem $ term CloseParentheses
equal            = lexem $ term Equal
semicolon        = lexem $ term Semicolon
dot              = lexem $ term Dot
comma            = lexem $ term Comma
space            = term Space

parenthesesed :: Syntax a -> Syntax a
parenthesesed p = pad openParentheses p closeParentheses

braced :: Syntax a -> Syntax a
braced p = pad openBrace p closeBrace

listOf :: Syntax a -> Syntax [ a ]
listOf = parenthesesed . joins comma

-- Keywords.

keyword :: String -> Syntax ()
keyword = lexem . term . Keyword

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

className' :: Syntax ClassName
className' = ClassName <$> identifier

methodName' :: Syntax MethodName
methodName' = MethodName <$> identifier

propertyName :: Syntax PropertyName
propertyName = PropertyName <$> identifier

variableName' :: Syntax VariableName
variableName' = VariableName <$> identifier

-- Top level.

grammar :: Syntax [ ClassDef ]
grammar = some ( lexem classDef )

-- Class level.

classDef :: Syntax ClassDef
classDef = ClassDef
  <$> classHead
  <*> braced ( many classTerm )

classHead :: Syntax ClassHead
classHead = ClassHead
  <$> ( classKeyword   >> className' )
  <*> ( extendsKeyword >> className' )

classTerm :: Syntax ClassTerm
classTerm = property <|> constructor <|> method

property :: Syntax ClassTerm
property = Property <$> className' <*> ( term Space >> propertyName << semicolon )

-- Constructor level.

constructor :: Syntax ClassTerm
constructor = Constructor
  <$> className'
  <*> listOf methodArg
  <*> braced constructorBody'

constructorBody' :: Syntax ConstructorBody
constructorBody' = ConstructorBody
  <$> ( superKeyword >> listOf propertyName << semicolon )
  <*> many selfAssignment

selfAssignment :: Syntax ( PropertyName, VariableName )
selfAssignment = (,)
  <$> ( thisKeyword >> dot >> propertyName )
  <*> ( equal >> variableName' << semicolon )

-- Method level.

method :: Syntax ClassTerm
method = MethodTerm
  <$> lexem className'
  <*> methodName'
  <*> listOf methodArg
  <*> braced ( returnKeyword >> expression << semicolon )

methodArg :: Syntax ( ClassName, VariableName )
methodArg = (,) <$> className' <*> ( space >> variableName' )

-- Expression level.

expression :: Syntax Expression
expression = lexem ( ( object <|> coercion <|> variable' ) >>= accessor )

coercion :: Syntax Expression
coercion = Coercion
  <$> parenthesesed className'
  <*> expression

variable' :: Syntax Expression
variable' = Variable <$> ( variableName' ! return "this" << thisKeyword )

accessor :: Expression -> Syntax Expression
accessor expr = ( dot >> identifier >>= invocation expr >>= accessor )
              ! return expr

invocation :: Expression -> String -> Syntax Expression
invocation expr name
  = MethodInvocation expr ( MethodName name ) <$> listOf expression
  ! return ( AttributeAccess expr ( PropertyName name ) )

object :: Syntax Expression
object = Object
  <$> ( newKeyword >> className' )
  <*> listOf expression
