module Youtan.Regex.Operators where

import Data.Char (isAlphaNum)

data Counter
  = KleeneStar
  | OneOrMore
  | ZeroOrOne
  deriving ( Show, Eq )

data Operator
  = Empty
  | Literal Char
  | Group Operator
  | Concatenation Operator Operator
  | Disjunction Operator Operator
  | Counts Operator Counter
  deriving ( Show, Eq )

data Parser
  = TopParser [ Operator ]
  | GroupParser [ Operator ]
  | DisjunctionParser
  deriving ( Show, Eq )

isCounter :: Char -> Bool
isCounter c = c `elem` "*+?"

isDisjunction :: Char -> Bool
isDisjunction = ( == ) '|'

isOpenGroup :: Char -> Bool
isOpenGroup = ( == ) '('

isCloseGroup :: Char -> Bool
isCloseGroup = ( == ) ')'

counter :: Char -> Counter
counter '*' = KleeneStar
counter '+' = OneOrMore
counter '?' = ZeroOrOne

parseString :: String -> Operator
parseString ""  = Empty
parseString str = parseTop ( TopParser [] ) str

buildCounter :: Operator -> Char -> Operator
buildCounter operator@( Literal _ ) ch = Counts operator ( counter ch )
buildCounter ( Disjunction b1 b2 ) ch = Disjunction b1 ( Counts b2 ( counter ch ) )
buildCounter ( Group operator ) ch = Counts operator ( counter ch )
-- The rest shouldn't appear.

parseGroup :: Parser -> String -> ( String, Operator )
parseGroup ( GroupParser [] )( x:xs )
  | isAlphaNum    x = parseGroup ( GroupParser [ Literal x ] ) xs
parseGroup ( GroupParser acc@( r:rs ) ) ( x:xs )
  | isAlphaNum    x = parseGroup ( GroupParser ( Literal x : acc ) ) xs
  | isCloseGroup  x = ( xs, Group ( foldl1 ( flip Concatenation ) acc ) )
  | isDisjunction x = parseGroup ( GroupParser ( Disjunction r operator : rs ) ) rest
    where ( rest, operator ) = parseDisjunction xs

parseTop :: Parser -> String -> Operator
parseTop ( TopParser acc ) [] = foldl1 ( flip Concatenation ) acc
parseTop ( TopParser [] ) ( x:xs )
  | isAlphaNum    x = parseTop ( TopParser [ Literal x ] ) xs
  | isOpenGroup   x = parseTop ( TopParser [ gOper ] ) gRest
    where
      ( gRest, gOper ) = parseGroup ( GroupParser [] ) xs
parseTop ( TopParser acc@( r:rs ) ) ( x:xs )
  | isAlphaNum    x = parseTop ( TopParser ( Literal x : acc ) ) xs
  | isCounter     x = parseTop ( TopParser ( buildCounter r x : rs ) ) xs
  | isOpenGroup   x = parseTop ( TopParser ( gOper : acc ) ) gRest
  | isDisjunction x = parseTop ( TopParser ( Disjunction r operator : rs ) ) rest
    where
      ( rest, operator ) = parseDisjunction xs
      ( gRest, gOper ) = parseGroup ( GroupParser [] ) xs
parseTop parser str = error $ show parser ++ show str

parseDisjunction :: String -> ( String, Operator )
parseDisjunction ( x:xs )
  | isAlphaNum x = ( xs, Literal x )
