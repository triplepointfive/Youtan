module Main where

import           Control.Monad.State
import           Data.Char ( chr, ord )
import qualified Data.Sequence as Seq

import           Youtan.Lexical.Tokenizer ( Rules, tokenizeDrops )
import           Youtan.Syntax.Parser ( term, (<<), many, runParser, Parser )

-- Lexical.

data Token
  = IncrP
  | DecrP
  | IncrB
  | DecrB
  | Out
  | In
  | Begin
  | End
  deriving ( Eq )

instance Show Token where
  show IncrP  = ">"
  show DecrP  = "<"
  show IncrB  = "+"
  show DecrB  = "-"
  show Out    = "."
  show In     = ","
  show Begin  = "["
  show End    = "]"

lexical :: String -> [ Token ]
lexical = tokenizeDrops rules drops
  where
    rules :: Rules Token
    rules =
      [ ( ">", const IncrP )
      , ( "<", const DecrP )
      , ( "\\+", const IncrB )
      , ( "-", const DecrB )
      , ( "\\.", const Out )
      , ( ",", const In )
      , ( "\\[", const Begin )
      , ( "\\]", const End )
      ]
    drops :: [ String ]
    drops = [ "[^-\\[\\]\\.\\,><\\+]+" ]

-- Syntax.

data Action
  = ChangeValue !Int
  | MoveForth
  | MoveBack
  | Output
  | Input
  | Loop [ Action ]
  deriving ( Eq )

instance Show Action where
  show ( ChangeValue n ) | n > 0     = replicate n '+'
                         | otherwise = replicate ( abs n ) '-'
  show MoveForth        = ">"
  show MoveBack         = "<"
  show Output           = "."
  show Input            = ","
  show ( Loop actions ) = "[" ++ concatMap show actions ++ "]"

syntax :: [ Token ] -> Either [ ( [ Action ], [ Token ] ) ] [ Action ]
syntax = runParser grammar
  where
    incrV, decrV, moveF, moveb, action :: Parser Token Action
    incrV  = const ( ChangeValue    1   ) <$> term IncrB
    decrV  = const ( ChangeValue ( -1 ) ) <$> term DecrB
    moveF  = const MoveForth <$> term IncrP
    moveb  = const MoveBack  <$> term DecrP
    output = const Output    <$> term Out
    input  = const Input     <$> term In
    loop   = term Begin >> ( Loop <$> grammar ) << term End
    action = msum [ incrV, decrV, moveF, moveb, output, input, loop ]

    grammar :: Parser Token [ Action ]
    grammar = many action

-- Semantic.

semantic :: Either [ ( [ Action ], [ Token ] ) ] [ Action ] -> [ Action ]
semantic ( Left x )  = error ( show x )
semantic ( Right l ) = case l of
                         [] -> []
                         ( x : xs ) -> group x xs
  where
    group :: Action -> [ Action ] -> [ Action ]
    group x [] = [ x ]
    group ( ChangeValue n ) ( ChangeValue x : xs ) = group ( ChangeValue ( n + x ) ) xs
    group x ( f : xs ) = x : group f xs

count :: [ Action ] -> Int
count [] = 0
count ( Loop a : xs ) = 1 + count a + count xs
count ( _ : xs ) = 1 + count xs

-- Interpret.

type Memory = ( Seq.Seq Int, Int, Seq.Seq Int )

interpret :: [ Action ] -> IO Memory
interpret actions = evalStateT ( mapM_ eval actions >> get ) newMemory
  where
    newMemory :: Memory
    newMemory = ( Seq.empty, 0, Seq.empty )

    eval :: Action -> StateT Memory IO ()
    eval ( ChangeValue n ) = modify ( second ( + n ) )
    eval MoveForth = do
      ( p, v, n ) <- get
      case Seq.viewl n of
        x Seq.:< xs -> put ( p Seq.|> v, x, xs )
        Seq.EmptyL  -> put ( p Seq.|> v, 0, n )
    eval MoveBack = do
      ( p, v, n ) <- get
      case Seq.viewr p of
        xs Seq.:> x -> put ( xs, x, v Seq.<| n )
        Seq.EmptyR  -> put (  p, 0, v Seq.<| n )
    eval Output = do
      ( _, v, _ ) <- get
      lift ( putChar ( chr v ) )
    eval Input = do
      v <- lift getChar
      modify ( second ( const ( ord v ) ) )
    eval l@( Loop list ) = do
      ( _, v, _ ) <- get
      when ( v /= 0 )
        ( mapM_ eval list >> eval l )

main :: IO ()
main = return ()

second :: ( b -> b ) -> ( a, b, c ) -> ( a, b, c )
second f ( a, b, c ) = ( a, f b, c )
