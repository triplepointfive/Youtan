-- | Implementation of NDFM.
{-# LANGUAGE RecordWildCards #-}

module Youtan.Regex.NDFM 
( FM( .. )
, NDFM( .. )
, State( .. )
, fromString
, longestMatch
, match
) where

import Control.Monad ( when )
import Control.Monad.State ( evalState, get, modify )
import qualified Control.Monad.State as State ( State )
import Data.List ( intercalate )
import qualified Data.Map.Strict as Map
import Data.Maybe ( isNothing, catMaybes )
import Data.String ( IsString( .. ) )

import Youtan.Regex.Operators ( Counter(..), Operator(..), parseString )
import Youtan.Regex.FM

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
with ( Empty _ ) lastID = NDFM lastID litID ( Map.fromList states )
  where
    litID = nextFreeID lastID
    states = [ ( lastID, emptyBranchState litID ), ( litID, emptyState ) ]
with ( Literal _ x ) lastID = NDFM lastID litID ( Map.fromList states )
  where
    litID = nextFreeID lastID
    states = [ ( lastID, branchState ( Exact x ) litID ), ( litID, emptyState ) ]
with ( CharClass _ chClass ) lastID = NDFM lastID litID ( Map.fromList states )
  where
    litID = nextFreeID lastID
    states = [ ( lastID, branchState ( Class chClass ) litID ), ( litID, emptyState ) ]
with ( Concatenation _ oper1 oper2 ) lastID = NDFM lastID finish2 allStates
  where
    ( NDFM _ finish1 states1 ) = with oper1 lastID
    ( NDFM _ finish2 states2 ) = with oper2 finish1
    allStates = Map.union states2 states1
with ( Disjunction _ oper1 oper2 ) lastID = NDFM lastID lastNodeID allStates
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
with ( Counts _ counter operator ) lastID =
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
instance IsString NDFM where
  fromString str = with ( parseString str ) initID

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
        symbolBranches = concatMap ( move ( tail input ) )
          $ matchState ( branches state ) ( head input )

-- | Returns the length of the longest matching substring of the input starting
-- from the beginning of string. Returns 'Nothing' if no matches.
longestMatch :: String -> Input -> Maybe Int
longestMatch regex str = evalState ( move 0 str ( startState ndfm ) >> get ) Nothing
  where
    ndfm = fromString regex

    upd :: Int -> Maybe Int -> Maybe Int
    upd n Nothing    = Just n
    upd n ( Just o ) = Just ( max o n )

    move :: Int -> Input -> StateID -> State.State ( Maybe Int ) [ StateID ]
    move currentLength input stateID = do
      when ( isFiniteState ndfm stateID ) ( modify ( upd currentLength ) )
      if null input
         then
           (:) stateID . concat <$> emptyBranches
         else do
           x <- symbolBranches
           y <- emptyBranches
           return ( concat ( x ++ y ) )
      where
        state = states ndfm Map.! stateID

        symbolBranches, emptyBranches :: State.State ( Maybe Int ) [ [ StateID ] ]
        emptyBranches =  mapM ( move currentLength input ) $
          catMaybes [ emptyBranch1 state, emptyBranch2 state ]

        symbolBranches = mapM ( move ( currentLength + 1 ) ( tail input ) )
          $ matchState ( branches state ) ( head input )
