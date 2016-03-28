{-# LANGUAGE RecordWildCards #-}

module Youtan.Regex.DFM where

import qualified Data.Map.Strict as Map

type Symbol = Char
type State = Int

type TransitionTable = Map.Map (State, Symbol) State

transit :: TransitionTable -> State -> Symbol -> State
transit table state symbol = table Map.! (state, symbol)

data DFM = DFM
  { finiteStates     :: ![State]
  , currentState     :: !State
  , transitionsTable :: !TransitionTable
  } deriving Show

move :: DFM -> Symbol -> DFM
move dfm@DFM{..} symbol
  = dfm{ currentState = transit transitionsTable currentState symbol }

finite :: DFM -> Bool
finite DFM{..} = currentState `elem` finiteStates

fromString :: String -> DFM
fromString str = DFM [ last states ] ( head states ) table
  where
    states = [0..length str]

match :: String -> Bool
match str = finite $ foldl move ( fromString str ) str
