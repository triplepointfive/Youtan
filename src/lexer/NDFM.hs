{-# LANGUAGE RecordWildCards #-}

module NDFM where

import qualified Data.Map.Strict as Map

import Data.Maybe ( catMaybes, maybeToList )
import Data.List ( intercalate )

type Symbol = Char
type Input  = String
type StateID = Int

nextFreeID :: StateID -> StateID
nextFreeID = succ

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = intercalate [] ( fmap f xs )

data State = State
  { branches     :: !( Map.Map Symbol StateID )
  , emptyBranch1 :: !( Maybe StateID )
  , emptyBranch2 :: !( Maybe StateID )
  } deriving Show

data NDFM = NDFM
  { finishState      :: !StateID
  , startState       :: !StateID
  , states           :: !( Map.Map StateID State )
  } deriving Show

finiteState :: NDFM -> StateID -> Bool
finiteState NDFM{..} = ( == ) finishState

fromString :: String -> NDFM
fromString str = undefined

match :: Input -> Bool
match str = any ( finiteState ndfm ) $ move str ( startState ndfm )
  where
    ndfm = undefined

    move :: Input -> StateID -> [ StateID ]
    move input stateID
      | null input = catMaybes emptyBranches
      | otherwise  = symbolBranches ++ flatMap (move input) (catMaybes emptyBranches)
      where
        state = states ndfm Map.! stateID
        emptyBranches = [ emptyBranch1 state, emptyBranch2 state ]

        symbolStateID :: Maybe StateID
        symbolStateID =  head input `Map.lookup` branches state

        symbolBranches :: [ StateID ]
        symbolBranches = maybeToList $ fmap ( move ( tail input ) ) symbolStateID

