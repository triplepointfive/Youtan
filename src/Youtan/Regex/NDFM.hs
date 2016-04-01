{-# LANGUAGE RecordWildCards #-}

module Youtan.Regex.NDFM where

import qualified Data.Map.Strict as Map

import Data.Maybe ( catMaybes, maybeToList )
import Data.List ( intercalate )

import Youtan.Regex.Operators ( Counter(..), Operator(..), parseString )

type Symbol = Char
type Input  = String
type StateID = Int

nextFreeID :: StateID -> StateID
nextFreeID = succ

initID :: StateID
initID = 0

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = intercalate [] ( fmap f xs )

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

data NDFM = NDFM
  { startState       :: !StateID
  , finishState      :: !StateID
  , states           :: !( Map.Map StateID State )
  } deriving Show

isFiniteState :: NDFM -> StateID -> Bool
isFiniteState NDFM{..} = ( == ) finishState

with :: Operator -> NDFM -> NDFM
with Empty NDFM{..} = NDFM startState litID ( Map.insert litID emptyState newStates)
  where
    litID = nextFreeID finishState
    newStates = Map.adjust
      (\ state -> state{ emptyBranch1 = Just litID } )
      finishState states
with ( Literal x ) NDFM{..} = NDFM startState litID ( Map.insert litID emptyState newStates)
  where
    litID = nextFreeID finishState
    newStates = Map.adjust
      (\ state -> state{ branches = Map.insert x litID ( branches state ) } )
      finishState states
with ( Concatenation oper1 oper2 ) ndfm = with oper2 ( with oper1 ndfm )

-- disjunction ( Disjunction oper1 oper2 ) =
  -- where
    -- n1 = with oper1 ( emptyNDFM initID )
    -- n2 = with oper2 ( emptyNDFM ( nextID $ finishState n1 ) )

emptyNDFM i = NDFM i i ( Map.singleton initID emptyState )

fromString :: String -> NDFM
fromString str = with ( parseString str ) ( emptyNDFM initID )

match :: String -> Input -> Bool
match regex str = matchNDFM ( fromString regex ) str

matchNDFM ndfm str = any ( isFiniteState ndfm ) $ move str ( startState ndfm )
  where
    move :: Input -> StateID -> [ StateID ]
    move input stateID
      | null input = stateID : catMaybes emptyBranches
      | otherwise  =
        symbolBranches ++
        flatMap (move input) (catMaybes emptyBranches)
      where
        state = states ndfm Map.! stateID

        emptyBranches = [ emptyBranch1 state, emptyBranch2 state ]

        symbolStateID :: Maybe StateID
        symbolStateID =  head input `Map.lookup` branches state

        symbolBranches :: [ StateID ]
        symbolBranches = maybe [] ( move ( tail input ) ) symbolStateID
