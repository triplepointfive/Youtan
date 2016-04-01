module Youtan.Regex.Operators where

import Data.Char (isAlphaNum)

-- | Represents a single character class.
data CharacterClass
  -- | Any char except for newline.
  = Dot
  -- | A word character ([a-zA-Z0-9_]).
  | Word
  -- | A digit character ([0-9]).
  | Digit
  -- | Single rule to revent any character class.
  | None CharacterClass
  deriving ( Show, Eq )

data Token
  = AlphaNum Char
  | Times Counter
  | Separator
  | OpenGroup
  | CloseGroup
  deriving ( Show, Eq )

type Tokens = [ Token ]

isCounter :: Char -> Bool
isCounter c = c `elem` "*+?"

isDisjunction :: Char -> Bool
isDisjunction = ( == ) '|'

isOpenGroup :: Char -> Bool
isOpenGroup = ( == ) '('

isCloseGroup :: Char -> Bool
isCloseGroup = ( == ) ')'

data Counter
  = KleeneStar
  | OneOrMore
  | ZeroOrOne
  deriving ( Show, Eq )

counter :: Char -> Counter
counter '*' = KleeneStar
counter '+' = OneOrMore
counter '?' = ZeroOrOne

data Operator
  = Empty
  | Literal Char
  | Concatenation Operator Operator
  | Disjunction Operator Operator
  | Counts Operator Counter
  deriving ( Show, Eq )

class Parser parser where
  parse :: parser -> String -> ( String, parser )
  parse parser [] = ( [], onStringEnd parser )
  parse parser ( x:xs )
    | isAlphaNum    x = onChar parser x xs
    | isOpenGroup   x = onOpenGroup parser xs
    | isCloseGroup  x = onCloseGroup parser xs
    | isDisjunction x = onDisjunction parser xs
    | isCounter     x = onCounter parser ( counter x ) xs

  onChar :: parser -> Char -> String -> ( String, parser )
  onCounter :: parser -> Counter -> String -> ( String, parser )

  onStringEnd :: parser -> parser

  onOpenGroup :: parser -> String -> ( String, parser )
  onCloseGroup :: parser -> String -> ( String, parser )
  onDisjunction :: parser -> String -> ( String, parser )

data TopParser = TopParser { oper :: Operator }
  deriving ( Show, Eq )

instance Parser TopParser where
  onStringEnd = id

  onChar ( TopParser operator ) x = parse ( TopParser ( operator `merge` Literal x ) )
  onCounter ( TopParser operator ) c = parse ( TopParser ( Counts operator c ) )
  onOpenGroup ( TopParser operator ) xs = parse ( TopParser ( operator `merge` gOper ) ) gRest
    where
      ( gRest, GroupParser gOper ) = parse ( GroupParser Empty ) xs
  onDisjunction ( TopParser operator ) xs = parse ( TopParser ( Disjunction operator dOper ) ) dRest
    where
      ( dRest, DisjunctionParser dOper ) = parse ( DisjunctionParser Empty ) xs
  onCloseGroup ( TopParser operator ) xs = error "Unexpected close group token"

data DisjunctionParser = DisjunctionParser Operator
  deriving ( Show, Eq )

instance Parser DisjunctionParser where
  onStringEnd = id

  onChar ( DisjunctionParser operator ) x = parse ( DisjunctionParser ( operator `merge` Literal x ) )
  onCloseGroup ( DisjunctionParser operator ) xs = ( xs, DisjunctionParser operator )
  onDisjunction ( DisjunctionParser operator ) xs = parse ( DisjunctionParser ( Disjunction operator dOper ) ) dRest
    where
      ( dRest, DisjunctionParser dOper ) = parse ( DisjunctionParser Empty ) xs

  onOpenGroup ( DisjunctionParser operator ) xs = error "Unexpected open group token"
  onCounter ( DisjunctionParser operator ) c xs = error "Unexpected counter token"

data GroupParser = GroupParser Operator
  deriving ( Show, Eq )

instance Parser GroupParser where
  onChar ( GroupParser operator ) x = parse ( GroupParser ( operator `merge` Literal x ) )
  onCloseGroup ( GroupParser operator ) xs = ( xs, GroupParser operator )
  onDisjunction ( GroupParser operator ) xs = ( dRest, GroupParser ( Disjunction operator dOper ) )
    where
      ( dRest, DisjunctionParser dOper ) = parse ( DisjunctionParser Empty ) xs

  onOpenGroup ( GroupParser operator ) xs = error "Unexpected open group token"
  onCounter ( GroupParser operator ) c xs = error "Unexpected counter token"
  onStringEnd = error "Unexpected end of line, expected close group token"

parseString :: String -> Operator
parseString = oper . snd . parse ( TopParser Empty )

tokenize :: String -> Tokens
tokenize = reverse . step []
  where
    step :: Tokens -> String -> Tokens
    step ts [] = ts
    step ts ( '+' : xs ) = step ( Times OneOrMore : ts ) xs
    step ts ( '*' : xs ) = step ( Times KleeneStar : ts ) xs
    step ts ( '?' : xs ) = step ( Times ZeroOrOne : ts ) xs
    step ts ( '|' : xs ) = step ( Separator : ts ) xs
    step ts ( '(' : xs ) = step ( OpenGroup : ts ) xs
    step ts ( ')' : xs ) = step ( CloseGroup : ts ) xs
    step ts ( x : xs )   = step ( AlphaNum x : ts ) xs

merge :: Operator -> Operator -> Operator
merge Empty operator      = operator
merge operator Empty      = operator
merge ( Disjunction Empty operator1 ) operator = Disjunction operator operator1
merge ( Disjunction operator1 Empty ) operator = Disjunction operator1 operator
merge operator1 operator2 = Concatenation operator1 operator2
