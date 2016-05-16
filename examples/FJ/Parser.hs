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
  , ( "[^ \n\t(){};=]+", Identifier )
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

type Properties = Map.Map PropertyName ClassName

type MethodArguments = Map.Map VariableName ClassName

type Methods = Map.Map MethodName MethodBody

data Class
  = Class
  { className   :: !ClassName
  , parentName  :: !ClassName
  , properties  :: !Properties
  , constructor :: !Constructor
  , methods     :: !Methods
  } deriving ( Show, Eq )

data MethodBody
  = MethodBody
  { returnType :: !ClassName
  , methodName :: !MethodName
  , arguments  :: !MethodArguments
  , body       :: !Expression
  }
  deriving ( Show, Eq )

data Constructor
  = Constructor
  { buildType       :: !ClassName
  , args            :: !Properties
  , superProperties :: ![ PropertyName ]
  , selfProperties  :: ![ ( PropertyName, PropertyName ) ]
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

property' :: Parser Token ( PropertyName, ClassName )
property' = do
  propType <- className'
  term Space
  propName <- propertyName'
  return ( propName, propType )

properties' :: Parser Token Properties
properties' = Map.fromList <$> many ( property' >>= \ s -> semicolon >> return s )

class' :: Parser Token Class
class' = do
  classKeyword
  selfName <- className'
  extendsKeyword
  parent <- className'
  Class selfName parent <$> return Map.empty <*> constructor' <*> return Map.empty

constructor' :: Parser Token Constructor
constructor' = do
  name <- className'
  pros <- Map.fromList <$> parenthesesed ( joins comma property' )
  ( superP, selfP ) <- braced $ do
    (,) <$> super <*> self
  return ( Constructor name pros superP selfP )
  where
    super = superKeyword >> parenthesesed ( joins comma propertyName' )
    self  = many $ do
      p1 <- thisKeyword >> dot >> propertyName'
      equal
      p2 <- propertyName'
      semicolon
      return ( p1, p2 )

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
space            = lexem $ term Space
equal            = lexem $ term Equal
semicolon        = lexem $ term Semicolon
dot              = lexem $ term Dot
comma            = lexem $ term Comma

parenthesesed :: Parser Token a -> Parser Token a
parenthesesed p = pad openParentheses p closeParentheses

braced :: Parser Token a -> Parser Token a
braced p = pad openBrace p closeBrace
