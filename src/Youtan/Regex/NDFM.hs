{-# LANGUAGE RecordWildCards #-}

module Youtan.Regex.NDFM where

import qualified Data.Map.Strict as Map

import Data.Maybe ( catMaybes, maybeToList, isNothing )
import Data.List ( intercalate )

import Youtan.Regex.Operators ( Counter(..), Operator(..), parseString )

type Symbol = Char
type Input  = String
type StateID = Int

nextFreeID :: StateID -> StateID
nextFreeID = succ

initID :: StateID
initID = 0

data State = State
  { branches     :: !( Map.Map Symbol StateID )
  , emptyBranch1 :: !( Maybe StateID )
  , emptyBranch2 :: !( Maybe StateID )
  } deriving Eq

instance Show State where
  show State{..} = concat ["{", intercalate ", " $ map (\ (s, i) -> s:" " ++ show i ) args  , "}"]
    where
      empties = map (\ stateID -> ( 'ε', stateID ) ) $ maybeToList emptyBranch1 ++ maybeToList emptyBranch2
      args = Map.toList branches ++ empties

emptyState :: State
emptyState = State Map.empty Nothing Nothing

emptyBranchState :: StateID -> State
emptyBranchState stateID = State Map.empty ( Just stateID ) Nothing

emptyBranchesState :: StateID -> StateID -> State
emptyBranchesState st1 st2 = State Map.empty ( Just st1 ) ( Just st2 )

branchState :: Symbol -> StateID -> State
branchState char stateID = State ( Map.singleton char stateID ) Nothing Nothing

setEmptyBranch :: StateID -> State -> State
setEmptyBranch stateID state@State{..}
  | isNothing emptyBranch1 = state{ emptyBranch1 = Just stateID }
  | isNothing emptyBranch2 = state{ emptyBranch2 = Just stateID }
  | otherwise = error $ "setEmptyBranch: Can't set empty branch: "
    ++ "both available branches are populated " ++ show state
    ++ " " ++ show stateID

data NDFM = NDFM
  { startState       :: !StateID
  , finishState      :: !StateID
  , states           :: !( Map.Map StateID State )
  } deriving Show

isFiniteState :: NDFM -> StateID -> Bool
isFiniteState NDFM{..} = ( == ) finishState

-- emptyNDFM i = NDFM i i ( Map.singleton initID emptyState )

with :: Operator -> StateID -> NDFM
with Empty lastID = NDFM lastID litID ( Map.fromList states )
  where
    litID = nextFreeID lastID
    states = [ ( lastID, emptyBranchState litID ), ( litID, emptyState ) ]
with ( Literal x ) lastID = NDFM lastID litID ( Map.fromList states )
  where
    litID = nextFreeID lastID
    states = [ ( lastID, branchState x litID ), ( litID, emptyState ) ]
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
    withFinish = foldl ( flip ( Map.adjust ( setEmptyBranch lastNodeID ) ) ) unionStates [ finish1, finish2 ]
    withLastNode = Map.insert lastNodeID emptyState withFinish
    allStates = Map.insert lastID ( emptyBranchesState node1StartID node2StartID ) withLastNode

fromString :: String -> NDFM
fromString str = with ( parseString str ) initID

match :: String -> Input -> Bool
match regex = matchNDFM ( fromString regex )

matchNDFM ndfm str = any ( isFiniteState ndfm ) $ move str ( startState ndfm )
  where
    move :: Input -> StateID -> [ StateID ]
    move input stateID
      | null input = stateID : emptyBranches
      | otherwise  = symbolBranches ++ emptyBranches
      where
        state = states ndfm Map.! stateID

        emptyBranches = concatMap ( move input ) $ catMaybes [ emptyBranch1 state, emptyBranch2 state ]

        symbolStateID :: Maybe StateID
        symbolStateID =  head input `Map.lookup` branches state

        symbolBranches :: [ StateID ]
        symbolBranches = maybe [] ( move ( tail input ) ) symbolStateID
