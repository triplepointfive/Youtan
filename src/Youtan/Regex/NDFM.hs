{-# LANGUAGE RecordWildCards #-}

module Youtan.Regex.NDFM where

import qualified Data.Map.Strict as Map

import Data.Maybe ( catMaybes, maybeToList )
import Data.List ( intercalate )

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
  } deriving Show

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

fromString :: String -> NDFM
fromString str = newNDFM
  where
    finishState = nextFreeID initID
    newStates = Map.fromList
      [ ( initID, emptyBranchState finishState )
      , ( finishState, emptyState )
      ]
    newNDFM = NDFM initID finishState newStates

match :: Input -> Bool
match str = any ( isFiniteState ndfm ) $ move str ( startState ndfm )
  where
    ndfm = fromString str

    move :: Input -> StateID -> [ StateID ]
    move input stateID
      | null input = catMaybes emptyBranches
      | otherwise  = symbolBranches ++
        flatMap (move input) (catMaybes emptyBranches)
      where
        state = states ndfm Map.! stateID
        emptyBranches = [ emptyBranch1 state, emptyBranch2 state ]

        symbolStateID :: Maybe StateID
        symbolStateID =  head input `Map.lookup` branches state

        symbolBranches :: [ StateID ]
        symbolBranches = maybe [] ( move ( tail input ) ) symbolStateID
