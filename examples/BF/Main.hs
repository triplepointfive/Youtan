module Main where

import           Control.Monad.State
import           Control.Arrow ( first, second )
import           Data.Char ( chr, ord )
import qualified Data.Map as M
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
    group x@( Loop [] ) ( f : xs ) = x : group f xs
    group ( Loop ( l : ls ) ) ( f : xs ) = ( Loop ( group l ls ) ) : group f xs
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

data Instr
  = ICall !String
  | IIncr !Int
  | IMvPt !Int
  | IPutC
  | IGetC
  deriving ( Show )

type Instrs = M.Map String [ Instr ]

type InstrB a = State ( Int, Instrs ) a

preproc :: [ Action ] -> Instrs
preproc list = evalState ( addMain >> ( snd <$> get ) ) ( 0, M.empty )
  where
    addMain :: InstrB ()
    addMain = do
      instrs <- mapM buildInstr list
      modify ( second ( M.insert "main" instrs ) )

    newID :: InstrB Int
    newID = do
      i <- fst <$> get
      modify ( first succ )
      return i

    buildInstr :: Action -> InstrB Instr
    buildInstr ( ChangeValue v ) = return ( IIncr v )
    buildInstr MoveForth         = return ( IMvPt 1 )
    buildInstr MoveBack          = return ( IMvPt ( -1 ) )
    buildInstr Output            = return IPutC
    buildInstr Input             = return IGetC
    buildInstr ( Loop acts )     = do
      name <- ( ( ++ ) "l" . show ) <$> newID
      instrs <- mapM buildInstr acts
      modify ( second ( M.insert name instrs ) )
      return ( ICall name )

int, i8 :: Type
int = IntegerType 32
i8  = IntegerType 8

memory :: Type
memory = ArrayType 100 int

val = cons . C.Int 32 . fromIntegral

codegenTop :: Instrs -> LLVM ()
codegenTop instrBlocks = do
  define VoidType "llvm.memset.p0i8.i64" [ ( pointer i8, UnName 0 )
                                         , ( i8, UnName 0 )
                                         , ( IntegerType 64, UnName 0 )
                                         , ( int, UnName 0 )
                                         , ( IntegerType 1, UnName 0 )
                                         ] []
  define int "putchar" [ ( int, UnName 0 ) ] []
  define int "getchar" [] []

  mapM_ ( \ ( name, coms ) ->
    define int name [ ( pointer int, Name "i" ), ( pointer memory, Name "m" )  ] ( bs name coms ) )
    ( M.toList $ M.delete "main" instrBlocks )

  define int "main" [] ( blks ( instrBlocks M.! "main" ) )
  where
    bs name coms = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry

      assign "i" ( local $ Name "i" )
      assign "m" ( local $ Name "m" )

      state <- loadState

      foldM_ cgen state coms

      -- ( _, _, mV ) <- loadState

      -- test <- cmp IP.EQ ( val 0 ) mV

      -- ifthen <- addBlock "if.then"
      -- ifelse <- addBlock "if.else"

      -- cbr test ifthen ifelse

      -- setBlock ifthen

      -- i <- getVar "i"
      -- m <- getVar "m"

      -- call ( externf ( Name name ) ) [ i, m ]
      -- br ifelse

      -- setBlock ifelse
      ret ( val 0 )

    blks exp = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry

      -- Position in a memory array.
      i <- alloca int
      assign "i" i
      store i ( val 0 )

      m <- alloca memory
      assign "m" m

      b <- bitCast m ( pointer i8 )

      void $ call ( externf ( Name "llvm.memset.p0i8.i64" ) )
        [ b
        , cons ( C.Int 8 0    )
        , cons ( C.Int 64 400 )
        , cons ( C.Int 32 32  )
        , cons ( C.Int 1 0    )
        ]

      state <- loadState
      foldM_ cgen state exp

      ret ( val 0 )

loadState :: Codegen ( Operand, Operand, Operand )
loadState = do
  i <- getVar "i"
  m <- getVar "m"

  iV <- load i
  mP <- getPtr m [ val 0, iV ]
  mV <- load mP

  return ( iV, mP, mV )

cgen :: ( Operand, Operand, Operand ) -> Instr -> Codegen ( Operand, Operand, Operand )
cgen ( i, p, mv ) ( IIncr x ) = do
  v <- add ( val x ) mv
  store p v
  return ( i, p, v )
cgen ( iP, _, _ ) ( IMvPt v ) = do
  i <- getVar "i"
  m <- getVar "m"

  nI <- add ( val v ) iP
  store i nI

  mP <- getPtr m [ val 0, nI ]
  mV <- load mP

  return ( nI, mP, mV )
cgen ( i, p, mv ) IPutC = do
  void $ call ( externf ( Name "putchar" ) ) [ mv ]
  return ( i, p, mv )
-- cgen ( _, _, _ ) ( ICall name ) = do
cgen v ( ICall name ) = do
  i <- getVar "i"
  m <- getVar "m"
  void $ call ( externf ( Name name ) ) [ i, m ]

  loadState

main :: IO ()
main = do
  [ file ] <- getArgs
  acts <- ( preproc . semantic . syntax . lexical ) <$> readFile file
  putStrLn ( "; " ++ show acts )
  void $ codegen initModule acts
  
  -- print ( preproc [ ChangeValue 97, MoveForth, MoveBack, Loop [ ChangeValue 1 ] ] )
  -- void $ codegen initModule ( preproc [ ChangeValue 97, MoveForth, MoveBack, Loop [ ChangeValue 1 ], Output ] )

  -- codegen initModule [ [ ChangeValue 97
                       -- , MoveForth, ChangeValue 2, Loop [ ChangeValue ( -1 ) ]
                       -- , MoveBack, Output ] ]-- [ acts ]
  where
    initModule = emptyModule "BF"

    liftError :: ExceptT String IO a -> IO a
    liftError = runExceptT >=> either fail return

    codegen mod fns = withContext $ \context ->
      liftError $ withModuleFromAST context newast $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr
        return newast
      where
        newast = runLLVM mod ( codegenTop fns )
