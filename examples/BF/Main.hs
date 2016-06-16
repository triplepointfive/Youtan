module Main where

import           Control.Monad.State
import           Data.Char ( chr, ord )
import qualified Data.Sequence as Seq
import           System.Environment ( getArgs )

import           Youtan.Lexical.Tokenizer ( Rules, tokenizeDrops )
import           Youtan.Syntax.Parser ( term, (<<), many, runParser, Parser )

import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC
import           Youtan.Compile.Codegen hiding ( term )
import qualified LLVM.General.AST.IntegerPredicate as IP

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

int, i8 :: Type
int = IntegerType 32
i8  = IntegerType 8

memory :: Type
memory = ArrayType 100 int

val = cons . C.Int 32 . fromIntegral

codegenTop :: [ Action ] -> LLVM ()
codegenTop exp = do
  define VoidType "llvm.memset.p0i8.i64" [ ( pointer i8, UnName 0 )
                                         , ( i8, UnName 0 )
                                         , ( IntegerType 64, UnName 0 )
                                         , ( int, UnName 0 )
                                         , ( IntegerType 1, UnName 0 )
                                         ] []
  define int "putchar" [ ( int, UnName 0 ) ] []
  define int "getchar" [] []

  define int "l1" [ ( pointer int, Name "i" ), ( pointer memory, Name "m" )  ] bs

  define int "main" [] blks
  where
    bs = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry

      iV <- load ( local $ Name "i" )
      mP <- getPtr ( local $ Name "m" ) [ val 0, iV ]
      mv <- load mP

      ifthen <- addBlock "if.then"
      ifelse <- addBlock "if.else"

      test <- cmp IP.NE ( val 0 ) mv
      cbr test ifthen ifelse

      setBlock ifelse
      test <- cmp IP.NE ( val 0 ) mv
      cbr test ifthen ifelse

      setBlock ifthen
      ret ( val 0 )


    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry

      -- Position in a memory array.
      i <- alloca int
      assign "i" i
      store i ( val 0 )

      m <- alloca memory
      assign "m" m

      b <- bitCast m ( pointer i8 )

      call ( externf ( Name "llvm.memset.p0i8.i64" ) )
        [ b
        , cons ( C.Int 8 0    )
        , cons ( C.Int 64 400 )
        , cons ( C.Int 32 32  )
        , cons ( C.Int 1 0    )
        ]

      iV <- load i
      mP <- getPtr m [ val 0, iV ]
      mV <- load mP

      foldM_ cgen ( iV, mP, mV ) exp

      ret ( val 0 )

cgen :: ( Operand, Operand, Operand ) -> Action -> Codegen ( Operand, Operand, Operand )
cgen ( i, p, mv ) ( ChangeValue x ) = do
  v <- add ( val x ) mv
  store p v
  return ( i, p, v )
cgen ( i, _, _ ) MoveForth = do
  iP <- getVar "i"
  nI <- add ( val 1 ) i
  store nI iP

  m <- getVar "m"
  mP <- getPtr m [ val 0, nI ]
  mV <- load mP

  return ( nI, mP, mV )
cgen ( i, _, _ ) MoveBack = do
  iP <- getVar "i"
  nI <- add ( val ( -1 ) ) i
  store nI iP

  m <- getVar "m"
  mP <- getPtr m [ val 0, nI ]
  mV <- load mP

  return ( nI, mP, mV )
cgen ( i, p, mv ) Output = do
  call ( externf ( Name "putchar" ) ) [ mv ]
  return ( i, p, mv )
cgen state@( i, p, mv ) ( Loop list ) = do
  ifthen <- addBlock ( "if.then" )
  ifelse <- addBlock ( "if.else" )
  ifexit <- addBlock ( "if.exit" )

  -- Entry.

  test <- cmp IP.NE ( val 0 ) mv
  cbr test ifthen ifelse

  -- ifthen.
  setBlock ifthen
  foldM_ cgen state list

  ifVal <- alloca int
  store ifVal ( val 0 )
  br ifexit
  ifthen <- getBlock

  -- ifelse.
  setBlock ifelse

  elseVal <- alloca int
  store elseVal ( val 1 )
  br ifexit
  ifelse <- getBlock

  -- ifexit.
  setBlock ifexit
  phi int [ ( ifVal, ifthen ), ( elseVal, ifelse ) ]

  return state

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
  -- [ file ] <- getArgs
  -- acts <- ( semantic . syntax . lexical ) <$> readFile file

  codegen initModule [ [ ChangeValue 97, Output ]]

  -- codegen initModule [ [ ChangeValue 97
                       -- , MoveForth, ChangeValue 2, Loop [ ChangeValue ( -1 ) ]
                       -- , MoveBack, Output ] ]-- [ acts ]
  return ()
  where
    initModule = emptyModule "BF"
