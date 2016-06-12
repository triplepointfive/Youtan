module Main where

import           Control.Monad.State
import           Data.Char ( chr, ord )
import qualified Data.Sequence as Seq

import           Youtan.Lexical.Tokenizer ( Rules, tokenizeDrops )
import           Youtan.Syntax.Parser ( term, (<<), many, runParser, Parser )

import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import           Youtan.Compile.Codegen hiding ( term )

import LLVM.General.Module
import LLVM.General.Context

import Control.Monad.Except

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

-- Interpret.

type Memory = ( Seq.Seq Int, Int, Seq.Seq Int )

interpret :: [ Action ] -> IO Memory
interpret actions = evalStateT ( mapM_ eval actions >> get ) newMemory
  where
    newMemory :: Memory
    newMemory = ( Seq.empty, 0, Seq.empty )

    second :: ( b -> b ) -> ( a, b, c ) -> ( a, b, c )
    second f ( a, b, c ) = ( a, f b, c )

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

-- Compiling.

int :: Type
int = IntegerType 32

-- initModule :: AST.Module
initModule = emptyModule "my cool jit"

codegenTop :: [ Action ] -> LLVM ()
codegenTop exp = do
  define int "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry

      -- Counter for position in a memory array.
      i <- alloca int
      cgen ( ChangeValue 0 ) >>= store i

      v <- alloca int
      c <- cgen ( ChangeValue 3 )
      store v c
      assign "a" v

      -- mapM_ cgen exp
      -- (return $ cons $ C.Int 32 0) >>= ret
      ( getvar "a" >>= load ) >>= ret

cgen :: Action -> Codegen Operand
cgen ( ChangeValue x ) = return $ cons $ C.Int 32 $ toInteger x
-- cgen (S.Float n) = return $ cons $ C.Float (F.Double n)


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return


-- codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn

main :: IO ()
main = do
  codegen initModule [ [ ChangeValue 3 ] ]
  return ()
  -- c <- codegen cM astModule []
  -- print c
  -- void $ codegen cM astModule []
