module FJ.Parser where

import Youtan.Lexical.Tokenizer
import Youtan.Syntax.Parser

import qualified Data.Map as Map

data Token
  = OpenBrace
  | CloseBrace
  | OpenParentheses
  | CloseParentheses
  | Space
  | Equal
  | Semicolon
  | Dot
  | Comma
  | Keyword { _keyword :: !String }
  | Identifier { _identifier :: !String }
  deriving ( Show, Eq )

rules :: Rules Token
rules =
  [ ( "\\{",  const OpenBrace )
  , ( "\\}",  const CloseBrace )
  , ( "\\(",  const OpenParentheses )
  , ( "\\)",  const CloseParentheses )
  , ( ";",    const Semicolon )
  , ( "=",    const Equal )
  , ( "\\.",  const Dot )
  , ( ",",    const Comma )
  , ( "\\s+", const Space )
  , ( "(new|class|extends|super|this|return)", Keyword )
  , ( "[^ \n\t(){};=,.]+", Identifier )
  ]

lexical :: String -> [ Token ]
lexical = tokenize rules

newtype ClassName = ClassName String
  deriving ( Show, Eq )

newtype MethodName = MethodName String
  deriving ( Show, Eq )

newtype PropertyName = PropertyName String
  deriving ( Show, Eq, Ord )

newtype VariableName = VariableName String
  deriving ( Show, Eq )

type Properties = [ ( ClassName, PropertyName ) ]

type MethodArguments = [ ( ClassName, VariableName ) ]

type Methods = Map.Map MethodName Method

data Class
  = Class
  { classHead :: !ClassHead
  , classBody :: !ClassBody
  } deriving ( Show, Eq )

data ClassHead
  = ClassHead
  { className  :: !ClassName
  , parentName :: !ClassName
  } deriving ( Show, Eq )

data ClassBody
  = ClassBody
  { properties  :: !Properties
  , constructor :: !Constructor
  , methods     :: ![ Method ]
  } deriving ( Show, Eq )

data Method
  = Method
  { returnType :: !ClassName
  , methodName :: !MethodName
  , arguments  :: !MethodArguments
  , body       :: !Expression
  }
  deriving ( Show, Eq )

data Constructor
  = Constructor
  { buildType       :: !ClassName
  , args            :: !MethodArguments
  , constructorBody :: !ConstructorBody
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
  | Object ClassName ![ Expression ]
  | Coercion !ClassName !Expression
  deriving ( Show, Eq )

-- syntax :: [ Token ] ->
-- syntax = runParser

isIdentifier :: Token -> Bool
isIdentifier ( Identifier _ ) = True
isIdentifier _ = False

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

property' :: Parser Token ( ClassName, PropertyName )
property' = (,) <$> className' <*> ( term Space >> propertyName' )

properties' :: Parser Token Properties
properties' = many ( property' `skip` semicolon )

class' :: Parser Token Class
class' = Class
  <$> classHead'
  <*> braced classBody'

classHead' :: Parser Token ClassHead
classHead' = ClassHead
  <$> ( classKeyword   >> className' )
  <*> ( extendsKeyword >> className' )

classBody' :: Parser Token ClassBody
classBody' = ClassBody
  <$> properties'
  <*> constructor'
  <*> many method

constructor' :: Parser Token Constructor
constructor' = Constructor
  <$> className'
  <*> parenthesesed methodArguments'
  <*> braced constructorBody'

methodArguments' :: Parser Token MethodArguments
methodArguments' = joins comma methodArg'

methodArg' :: Parser Token ( ClassName, VariableName )
methodArg' = (,) <$> className' <*> ( space >> variableName' )

constructorBody' :: Parser Token ConstructorBody
constructorBody' = ConstructorBody
  <$> ( superKeyword >> parenthesesed ( joins comma propertyName' ) `skip` semicolon )
  <*> many selfAssignment

selfAssignment :: Parser Token ( PropertyName, VariableName )
selfAssignment = (,)
  <$> ( thisKeyword >> dot >> propertyName' )
  <*> ( equal >> variableName' `skip` semicolon )

newKeyword, classKeyword, extendsKeyword :: Parser Token ()
superKeyword, thisKeyword, returnKeyword :: Parser Token ()
newKeyword     = keyword "new"
classKeyword   = keyword "class"
extendsKeyword = keyword "extends"
superKeyword   = keyword "super"
thisKeyword    = keyword "this"
returnKeyword  = keyword "return"

keyword :: String -> Parser Token ()
keyword = lexem . term . Keyword

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

expression :: Parser Token Expression
expression = lexem ( ( object <|> coercion' <|> variable' ) >>= accessor )

coercion' :: Parser Token Expression
coercion' = Coercion
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

method :: Parser Token Method
method = Method
  <$> lexem className'
  <*> methodName'
  <*> parenthesesed methodArguments'
  <*> braced ( returnKeyword >> ( expression `skip` semicolon ) )

grammar :: Parser Token [ Class ]
grammar = some ( lexem class' )
