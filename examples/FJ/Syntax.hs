module Syntax
( Class ( .. )
, ClassHead ( .. )
, ClassTerm ( .. )
, ConstructorBody ( .. )
, Expression ( .. )

, ClassName ( .. )
, MethodName ( .. )
, PropertyName ( .. )
, VariableName ( .. )

, expression
, syntax
, with
) where

import Youtan.Syntax.Parser

import Lexical ( Token( .. ) )

-- import qualified Data.Map as Map

newtype ClassName = ClassName String
  deriving ( Show, Eq )

newtype MethodName = MethodName String
  deriving ( Show, Eq )

newtype PropertyName = PropertyName String
  deriving ( Show, Eq, Ord )

newtype VariableName = VariableName String
  deriving ( Show, Eq )

-- type Properties = [ ( ClassName, PropertyName ) ]

type MethodArguments = [ ( ClassName, VariableName ) ]

-- type Methods = Map.Map MethodName Method

syntax :: [ Token ] -> Either [([Class], [Token])] [Class]
syntax = runParser grammar

with :: Parser a b -> [ a ] -> Either [ ( b, [ a ] ) ] b
with = runParser

data Class = Class !ClassHead ![ ClassTerm ]
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
  | Method
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

lexem :: Parser Token a -> Parser Token a
lexem p = pad ( opt Space ) p ( opt Space )

openBrace, closeBrace, semicolon, dot, space :: Parser Token ()
openParentheses, closeParentheses, equal, comma :: Parser Token ()
openBrace        = lexem $ term OpenBrace
closeBrace       = lexem $ term CloseBrace
openParentheses  = lexem $ term OpenParentheses
closeParentheses = lexem $ term CloseParentheses
equal            = lexem $ term Equal
semicolon        = lexem $ term Semicolon
dot              = lexem $ term Dot
comma            = lexem $ term Comma
space            = term Space

parenthesesed :: Parser Token a -> Parser Token a
parenthesesed p = pad openParentheses p closeParentheses

braced :: Parser Token a -> Parser Token a
braced p = pad openBrace p closeBrace

-- Keywords.

keyword :: String -> Parser Token ()
keyword = lexem . term . Keyword

newKeyword, classKeyword, extendsKeyword :: Parser Token ()
superKeyword, thisKeyword, returnKeyword :: Parser Token ()
newKeyword     = keyword "new"
classKeyword   = keyword "class"
extendsKeyword = keyword "extends"
superKeyword   = keyword "super"
thisKeyword    = keyword "this"
returnKeyword  = keyword "return"

-- Strings.

identifier :: Parser Token String
identifier = _identifier <$> satisfy isIdentifier

className' :: Parser Token ClassName
className' = ClassName <$> identifier

methodName' :: Parser Token MethodName
methodName' = MethodName <$> identifier

propertyName' :: Parser Token PropertyName
propertyName' = PropertyName <$> identifier

variableName' :: Parser Token VariableName
variableName' = VariableName <$> identifier

-- Top level.

grammar :: Parser Token [ Class ]
grammar = some ( lexem class' )

-- Class level.

class' :: Parser Token Class
class' = Class
  <$> classHead
  <*> braced ( many classTerm )

classHead :: Parser Token ClassHead
classHead = ClassHead
  <$> ( classKeyword   >> className' )
  <*> ( extendsKeyword >> className' )

classTerm :: Parser Token ClassTerm
classTerm = property <|> constructor <|> method

property :: Parser Token ClassTerm
property = Property <$> className' <*> ( term Space >> propertyName' << semicolon )

-- Constructor level.

constructor :: Parser Token ClassTerm
constructor = Constructor
  <$> className'
  <*> parenthesesed methodArguments'
  <*> braced constructorBody'

constructorBody' :: Parser Token ConstructorBody
constructorBody' = ConstructorBody
  <$> ( superKeyword >> parenthesesed ( joins comma propertyName' ) << semicolon )
  <*> many selfAssignment

selfAssignment :: Parser Token ( PropertyName, VariableName )
selfAssignment = (,)
  <$> ( thisKeyword >> dot >> propertyName' )
  <*> ( equal >> variableName' << semicolon )

-- Method level.

method :: Parser Token ClassTerm
method = Method
  <$> lexem className'
  <*> methodName'
  <*> parenthesesed methodArguments'
  <*> braced ( returnKeyword >> expression << semicolon )

methodArguments' :: Parser Token MethodArguments
methodArguments' = joins comma methodArg'

methodArg' :: Parser Token ( ClassName, VariableName )
methodArg' = (,) <$> className' <*> ( space >> variableName' )

-- Expression level.

expression :: Parser Token Expression
expression = lexem ( ( object <|> coercion <|> variable' ) >>= accessor )

coercion :: Parser Token Expression
coercion = Coercion
  <$> parenthesesed className'
  <*> expression

variable' :: Parser Token Expression
variable' = ( Variable <$> variableName' )
  <|> ( const ( Variable ( VariableName "this" ) ) <$> thisKeyword )

accessor :: Expression -> Parser Token Expression
accessor expr =
  ( ( AttributeAccess <$> return expr <*> ( dot >> propertyName' ) ) >>= accessor )
  <|> return expr

object :: Parser Token Expression
object = Object
  <$> ( newKeyword >> className' )
  <*> parenthesesed ( joins comma expression )

-- | MethodInvocation !Expression !MethodName ![ Expression ]
-- | Object ClassName ![ Expression ]
