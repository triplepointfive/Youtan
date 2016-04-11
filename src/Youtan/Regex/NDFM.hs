-- | Implementation of NDFM.
{-# LANGUAGE RecordWildCards #-}

module Youtan.Regex.NDFM where

import qualified Data.Map.Strict as Map

import Data.Maybe ( catMaybes, isNothing, catMaybes )
import Data.List ( intercalate )
import Data.Char ( isDigit, isAscii, isSpace )

import Youtan.Regex.Operators ( Counter(..), Operator(..), CharacterClass(..), parseString )

-- | A literal in input string.
type Symbol = Char

-- | An input string.
type Input  = String

-- | ID of a node, must be uniq within 'NDFM'.
type StateID = Int

-- | Returns next ID in a chain.
nextFreeID :: StateID -> StateID
nextFreeID = succ

-- | Just some ID to start off.
initID :: StateID
initID = 0

-- | Extension for char matching, allows to match with 'CharacterClass'.
data Matcher
  = Exact Symbol         -- ^ Single char.
  | Class CharacterClass -- ^ Any 'CharacterClass'.
  deriving ( Eq )

-- | For better display.
instance Show Matcher where
  show ( Exact ch ) = [ ch ]
  show ( Class chClass ) = show chClass

-- | A Node in 'NDFM' graph.
data State = State
  { branches     :: ![ ( Matcher, StateID ) ] -- ^ Connections to other nodes.
  , emptyBranch1 :: !( Maybe StateID )        -- ^ Empty connection.
  , emptyBranch2 :: !( Maybe StateID )        -- ^ Empty connection.
  } deriving Eq

-- | For better display.
instance Show State where
  show State{..} = concat ["{", intercalate ", " $ map (\ (s, i) -> show s ++ " " ++ show i ) args  , "}"]
    where
      empties = map (\ stateID -> ( Exact 'ε', stateID ) ) $ catMaybes [ emptyBranch1, emptyBranch2 ]
      args = branches ++ empties

-- | Brand new node with no connections.
emptyState :: State
emptyState = State [] Nothing Nothing

-- | Node with a single connection to the given 'StateID'.
emptyBranchState :: StateID -> State
emptyBranchState stateID = State [] ( Just stateID ) Nothing

-- | Node with a two connections to given 'StateID's.
emptyBranchesState :: StateID -> StateID -> State
emptyBranchesState st1 st2 = State [] ( Just st1 ) ( Just st2 )

-- | Node with a matcher to the given 'StateID's.
branchState :: Matcher -> StateID -> State
branchState matcher stateID = State [ ( matcher, stateID ) ] Nothing Nothing

-- | Sets new empty connection to node.
-- Fails if the node already has two empty branches.
setEmptyBranch :: StateID -> State -> State
setEmptyBranch stateID state@State{..}
  | isNothing emptyBranch1 = state{ emptyBranch1 = Just stateID }
  | isNothing emptyBranch2 = state{ emptyBranch2 = Just stateID }
  | otherwise = error $ "setEmptyBranch: Can't set empty branch: "
    ++ "both available branches are populated " ++ show state
    ++ " " ++ show stateID

-- | Mapping of 'StateID' to node itself.
type States = Map.Map StateID State

-- | Represents nondeterministic finite automaton.
data NDFM = NDFM
  { startState  :: !StateID
  , finishState :: !StateID
  , states      :: !States
  } deriving Show

-- | Checks if the given 'StateID' matches finite state of 'NDFM'.
isFiniteState :: NDFM -> StateID -> Bool
isFiniteState NDFM{..} = ( == ) finishState

-- | Builder of NDFM according to operators tree
with :: Operator -> StateID -> NDFM
with Empty lastID = NDFM lastID litID ( Map.fromList states )
  where
    litID = nextFreeID lastID
    states = [ ( lastID, emptyBranchState litID ), ( litID, emptyState ) ]
with ( Literal x ) lastID = NDFM lastID litID ( Map.fromList states )
  where
    litID = nextFreeID lastID
    states = [ ( lastID, branchState ( Exact x ) litID ), ( litID, emptyState ) ]
with ( CharClass chClass ) lastID = NDFM lastID litID ( Map.fromList states )
  where
    litID = nextFreeID lastID
    states = [ ( lastID, branchState ( Class chClass ) litID ), ( litID, emptyState ) ]
with ( Concatenation oper1 oper2 ) lastID = NDFM lastID finish2 allStates
  where
    ( NDFM _ finish1 states1 ) = with oper1 lastID
    ( NDFM _ finish2 states2 ) = with oper2 finish1
    allStates = Map.union states2 states1
with ( Disjunction oper1 oper2 ) lastID = NDFM lastID lastNodeID allStates
  where
    node1StartID = nextFreeID lastID
    node2StartID = nextFreeID finish1
    ( NDFM _ finish1 states1 ) = with oper1 node1StartID
    ( NDFM _ finish2 states2 ) = with oper2 node2StartID
    lastNodeID = nextFreeID finish2
    unionStates = Map.union states2 states1
    withFinish = foldl ( flip ( `link` lastNodeID  ) ) unionStates [ finish1, finish2 ]
    withLastNode = Map.insert lastNodeID emptyState withFinish
    allStates = Map.insert lastID ( emptyBranchesState node1StartID node2StartID ) withLastNode
with ( Counts counter operator ) lastID =
  countNode counter ( with operator ( nextFreeID lastID ) ) lastID
with ( Group operator ) lastID = with operator lastID

-- | Constructs 'NDFM' for counter node.
countNode :: Counter -> NDFM -> StateID -> NDFM
countNode counter NDFM{..} lastID = NDFM lastID endID $ case counter of
    KleeneStar -> link lastID endID $ baseStates $ link finishState startState states
    ZeroOrOne  -> link lastID endID $ baseStates   states
    OneOrMore  ->                     baseStates $ link finishState startState states
  where
    endID = nextFreeID finishState
    baseStates
      = link finishState endID
      . link lastID startState
      . Map.insert endID emptyState
      . Map.insert lastID emptyState

-- | Connects two nodes.
link :: StateID -> StateID -> States -> States
link from to = Map.adjust ( setEmptyBranch to ) from

-- | Builds 'NDFM' from string.
fromString :: String -> NDFM
fromString str = with ( parseString str ) initID

-- | Search for matching branches within a node. Includes brank branches.
matchState :: State -> Symbol -> [ StateID ]
matchState State{..} char = map snd $ filter ( matches . fst ) branches
  where
    matches ( Exact x )    = x == char
    matches ( Class Dot )  = True
    matches ( Class Word ) = any ( \f -> f char ) [ isAscii, isDigit, (==) '_' ]
    matches ( Class Digit )      = isDigit char
    matches ( Class Whitespace ) = isSpace char
    matches ( Class ( None chClass ) ) = not $ matches ( Class chClass )

-- | Tries to apply regex to an input string.
match :: String -> Input -> Bool
match regex = matchNDFM ( fromString regex )

-- | Checks whether 'NDFM' accepts given string.
matchNDFM :: NDFM -> Input -> Bool
matchNDFM ndfm str = any ( isFiniteState ndfm ) $ move str ( startState ndfm )
  where
    move :: Input -> StateID -> [ StateID ]
    move input stateID
      | null input = stateID : emptyBranches
      | otherwise  = symbolBranches ++ emptyBranches
      where
        state = states ndfm Map.! stateID

        emptyBranches = concatMap ( move input ) $
          catMaybes [ emptyBranch1 state, emptyBranch2 state ]

        symbolBranches :: [ StateID ]
        symbolBranches = concatMap ( move ( tail input ) ) $ matchState state ( head input )
