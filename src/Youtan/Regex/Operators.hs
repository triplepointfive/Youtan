-- | Module for parsing strings into an operators tree according to
-- base regex rules.
module Youtan.Regex.Operators where

import Data.Tuple ( swap )

-- | Represents a single character class.
data CharacterClass
  -- | Any char except for newline.
  = Dot
  -- | A word character ([a-zA-Z0-9_]).
  | Word
  -- | A digit character ([0-9]).
  | Digit
  -- | A whitespace character ([ \t\r\n\f]).
  | Whitespace
  -- | Single rule to revent any character class.
  | None CharacterClass
  -- | Group of chars.
  | CharGroup String
  deriving ( Show, Eq )

-- | A token in regex.
data Token
  -- | Arbitrary 'Char'.
  = Character Char
  -- | Character class, like a digit, space etc.
  | Class CharacterClass
  -- | Repeater.
  | Times Counter
  -- | Works as disjunction.
  | Separator
  -- | Openning new group.
  | OpenGroup
  -- | Closing current group.
  | CloseGroup
  deriving ( Show, Eq )

-- | Shortcut for 'Token' list. Might be replaced with another structure
-- later on.
type Tokens = [ Token ]

-- | Repeater - how many times operator migth appear.
data Counter
  -- | Any number of times - from 0 up to inf.
  = KleeneStar
  -- | At least one time.
  | OneOrMore
  -- | Optional, but one time at most.
  | ZeroOrOne
  deriving ( Eq )

-- | For better display.
instance Show Counter where
  show KleeneStar = "*"
  show OneOrMore = "+"
  show ZeroOrOne = "?"

-- | Action on matching input.
data Operator
  -- | Blank operator, for empty strings etc. Matches nothing only.
  = Empty
  -- | Single arbitrary character.
  | Literal Char
  -- | A sequence of two operators.
  | Concatenation Operator Operator
  -- | A separator, means at least one branch must match.
  | Disjunction Operator Operator
  -- | Counter on 'Operator', could go from 0 up to inf.
  | Counts Counter Operator
  -- | A set of literals.
  | CharClass CharacterClass
  -- | A union of operators. Could be used to apply something on a set of them.
  | Group Operator
  deriving ( Show, Eq )

-- | When one counter is applied to another that doesn't make much sense to
-- treat them separately. Could be traversed to one counter instead.
mergeCounters :: Counter -> Counter -> Counter
mergeCounters ZeroOrOne ZeroOrOne = ZeroOrOne
mergeCounters OneOrMore OneOrMore = OneOrMore
-- All the rest combinations lead to kleene.
mergeCounters _ _                 = KleeneStar

-- | Class means visitor over a token list and building an operators tree.
class Parser parser where
  -- | Builds a tree with given parser and string.
  build :: parser -> Tokens -> Operator
  build parser = getOperator . snd . parse ( setOperator parser Empty )

  -- | Top parsing function, dispense operators over other functions.
  parse :: parser -> Tokens -> ( Tokens, parser )
  parse parser [] = ( [], onStringEnd parser )
  parse parser ( x:xs ) = process parser xs
    where
      process = case x of
        Character c -> onChar c
        OpenGroup   -> onOpenGroup
        CloseGroup  -> onCloseGroup
        Separator   -> onDisjunction
        Times c     -> onCounter c
        Class c     -> onClass c

  -- | Action for 'Class' token.
  onClass :: CharacterClass -> parser -> Tokens -> ( Tokens, parser )
  onClass charClass = action ( `merge` CharClass charClass )

  -- | Action for 'Character' token.
  onChar :: Char -> parser -> Tokens -> ( Tokens, parser )
  onChar x = action ( `merge` Literal x )

  -- | Action for 'Times' token.
  onCounter :: Counter -> parser -> Tokens -> ( Tokens, parser )
  onCounter c = action count
    where
      -- | Applies counter to an operator. The only thing counter respects is
      -- concationation, all the rest could be wrapped.
      count :: Operator -> Operator
      count ( Concatenation oper1 oper2 ) = Concatenation oper1 ( count oper2 )
      count ( Group counter@( Counts _ _ ) ) = Group ( count counter )
      count ( Counts counter oper ) = Counts ( mergeCounters c counter ) oper
      count Empty    = error $
        "Target of repeat operator " ++ show c ++ " is not specified"
      count operator = Counts c operator

  -- | Action for 'Separate' token.
  onDisjunction :: parser -> Tokens -> ( Tokens, parser )
  onDisjunction parser xs = ( rest, mapOperator parser ( `Disjunction` oper ) )
    where
      ( rest, DisjunctionParser oper ) = parse ( DisjunctionParser Empty ) xs

  -- | Action for 'OpenGroup' token.
  onOpenGroup :: parser -> Tokens -> ( Tokens, parser )

  -- | Action for 'CloseGroup' token.
  onCloseGroup :: parser -> Tokens -> ( Tokens, parser )

  -- | Action on end of input line. Some parsers might want to fail if end is
  -- unexpected.
  onStringEnd :: parser -> parser

  -- | Operator getter.
  getOperator :: parser -> Operator

  -- | Operator setter.
  setOperator :: parser -> Operator -> parser

  -- | Applies a function on operator.
  mapOperator :: parser -> ( Operator -> Operator ) -> parser
  mapOperator parser f = setOperator parser ( f ( getOperator parser ) )

  -- | Applies a function on operator and calls 'parse' on result.
  action :: ( Operator -> Operator ) -> parser -> Tokens -> ( Tokens, parser )
  action fun parser = parse ( mapOperator parser fun )

-- | Top level parser, should be applied for new string.
data TopParser = TopParser Operator
  deriving ( Show, Eq )

-- | Common parser, fails on 'CloseGroup' and that\'s it.
instance Parser TopParser where
  onStringEnd = id

  onOpenGroup ( TopParser operator ) xs = parse ( TopParser ( operator `merge` Group gOper ) ) gRest
    where ( gRest, GroupParser gOper ) = parse ( GroupParser Empty ) xs
  onCloseGroup = error "Unexpected close group token"

  getOperator ( TopParser operator ) = operator
  setOperator ( TopParser _ ) = TopParser

-- | Parser for the case separator is met.
data DisjunctionParser = DisjunctionParser Operator
  deriving ( Show, Eq )

-- | Parses operators up to close group token.
instance Parser DisjunctionParser where
  onStringEnd = id

  onCloseGroup = curry swap

  onOpenGroup = error "Unexpected open group token"

  getOperator ( DisjunctionParser operator ) = operator
  setOperator ( DisjunctionParser _ ) = DisjunctionParser

-- | Wrapping parser, determines a scope for operators.
data GroupParser = GroupParser Operator
  deriving ( Show, Eq )

-- | Parses operators up to close group token. Fails on input end.
instance Parser GroupParser where
  onCloseGroup ( GroupParser operator ) xs = ( xs, GroupParser operator )

  onOpenGroup = error "Unexpected open group token"
  onStringEnd = error "Unexpected end of line, expected close group token"

  getOperator ( GroupParser operator ) = operator
  setOperator ( GroupParser _ ) = GroupParser

-- | Translates a regex looking like string into a tree of operators.
parseString :: String -> Operator
parseString = build ( TopParser Empty ) . tokenize

-- | Does exactly what it says on the tin.
tokenize :: String -> Tokens
tokenize = reverse . step []
  where
    -- | Separate function to make it tail recursive.
    step :: Tokens -> String -> Tokens
    step ts [] = ts
    step ts ( '+' : xs )  = step ( Times OneOrMore : ts ) xs
    step ts ( '*' : xs )  = step ( Times KleeneStar : ts ) xs
    step ts ( '?' : xs )  = step ( Times ZeroOrOne : ts ) xs
    step ts ( '|' : xs )  = step ( Separator : ts ) xs
    step ts ( '(' : xs )  = step ( OpenGroup : ts ) xs
    step ts ( ')' : xs )  = step ( CloseGroup : ts ) xs
    step ts ( '.' : xs )  = step ( Class Dot : ts ) xs
    step ts ( '[' : '^' : xs )
      = step ( Class ( None ( CharGroup group ) ) : ts ) rest
      where ( group, rest ) = charClass "" xs
    step ts ( '[' : xs )  = step ( Class ( CharGroup group ) : ts ) rest
      where ( group, rest ) = charClass "" xs
    step ts ( '\\' : xs ) = escaped ts xs
    step ts ( x : xs )    = step ( Character x : ts ) xs

    -- | Separate function for escaped sequence. Just to make 'step'
    -- more readable.
    escaped :: Tokens -> String -> Tokens
    escaped _  []           = error "Too short escape sequence"
    escaped ts ( 'w' : xs ) = step ( Class Word : ts ) xs
    escaped ts ( 'd' : xs ) = step ( Class Digit : ts ) xs
    escaped ts ( 's' : xs ) = step ( Class Whitespace : ts ) xs
    escaped ts ( 'W' : xs ) = step ( Class ( None Word ) : ts ) xs
    escaped ts ( 'D' : xs ) = step ( Class ( None Digit ) : ts ) xs
    escaped ts ( 'S' : xs ) = step ( Class ( None Whitespace ) : ts ) xs
    escaped ts (   x : xs ) = step ( Character x : ts ) xs

    charClass :: String -> String -> ( String, String )
    charClass _ [] = error "Premature end of char-class"
    -- TODO: Warn of empty group.
    charClass g ( x : '-' : y : xs )  = charClass ( [ x .. y ] ++ g ) xs
    charClass group ( '\\' : x : xs ) = charClass ( x : group ) xs
    charClass group ( ']' : xs )      = ( group, xs )
    charClass group ( x : xs )        = charClass ( x : group ) xs

-- | Kludge for parsers with no operator ('Empty') yet. Kinda blame function,
-- but much better than wrapping it with 'Maybe'.
merge :: Operator -> Operator -> Operator
merge Empty operator      = operator
merge operator Empty      = operator
merge ( Disjunction Empty disOper ) operator = Disjunction operator disOper
merge ( Disjunction disOper Empty ) operator = Disjunction disOper operator
merge operator1 operator2 = Concatenation operator1 operator2
