{-# LANGUAGE RecordWildCards #-}

module Youtan.Regex.DFM where

import Data.Maybe ( catMaybes, listToMaybe )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Youtan.Regex.NDFM as NDFM ( State( .. ), NDFM( .. ), StateID, Matcher( .. ), matchState )

type Symbol = Char
type State = Char

initID :: State
initID = 'a'

nextFreeID :: State -> State
nextFreeID = succ

type DState = Set.Set NDFM.StateID

data DTable
  = DTable
  { table           :: !( Map.Map ( DState, NDFM.Matcher ) ( Set.Set NDFM.StateID ) )
  , processedStates :: !( Set.Set DState )
  , states2         :: !( Set.Set DState )
  , finishState     :: !NDFM.StateID
  } deriving ( Eq, Show )

type TransitionTable = Map.Map ( State, NDFM.Matcher ) State

data DFM = DFM
  { finiteStates     :: ![ State ]
  , currentState     :: !( Maybe State )
  , transitionsTable :: !TransitionTable
  } deriving Show

fromNDFM :: NDFM.NDFM -> DFM
fromNDFM = buildDFM . buildDTable

markState :: DTable -> ( DState, DTable )
markState dtable@DTable{..} = ( Set.elemAt 0 states2,
  dtable{ states2 = Set.deleteAt 0 states2,
          processedStates = Set.insert ( Set.elemAt 0 states2 ) processedStates }
                              )
buildDFM :: DTable -> DFM
buildDFM DTable{..} = DFM finish ( Just initID ) finStates
  where
    finish :: [ State ]
    finish = map snd $ Map.toList $ Map.filterWithKey ( \ k _ -> Set.member finishState k ) states

    states :: Map.Map ( Set.Set NDFM.StateID ) State
    states = Map.fromList $ zip ( Set.toList processedStates ) ( iterate nextFreeID initID )

    finStates = Map.fromList $
      map ( \(( from, m ), to ) -> ( ( states Map.! from , m ), states Map.! to ) ) $
      Map.toList table

buildDTable :: NDFM.NDFM -> DTable
buildDTable NDFM.NDFM{..} = step newDTable
  where
    newDTable = DTable Map.empty
      Set.empty
      ( Set.singleton $ eClosure ( Set.singleton startState ) )
      finishState

    eClosure :: Set.Set NDFM.StateID -> Set.Set NDFM.StateID
    eClosure = Set.foldl Set.union Set.empty . Set.map closure
      where
        closure :: NDFM.StateID -> Set.Set NDFM.StateID
        closure stateID =  Set.insert stateID $
          eClosure ( Set.fromList $ catMaybes [ NDFM.emptyBranch1 state, NDFM.emptyBranch2 state ] )
          where state = states Map.! stateID

    move :: NDFM.Matcher -> DState -> Set.Set NDFM.StateID
    move m =
        Set.map head
      . Set.filter ( not . null )
      . Set.map ( map snd . filter ( (==) m . fst ) . NDFM.branches . ( Map.! ) states )

    symbols :: DState -> Set.Set NDFM.Matcher
    symbols = Set.foldl Set.union Set.empty .
      Set.map ( Set.fromList . map fst . NDFM.branches . ( Map.! ) states )

    step :: DTable -> DTable
    step dtable2@DTable{..}
      | Set.null states2 = dtable2
      | otherwise        = step ( foldl ( update state ) dTable ( symbols state ) )
      where
        ( state, dTable ) = markState dtable2

    update :: DState -> DTable -> NDFM.Matcher -> DTable
    update state d a = up{ table = Map.insert ( state, a ) u ( table d ) }
      where
        u = eClosure ( move a state )
        up = if Set.member u ( processedStates d )
             then d
             else d{ states2 = Set.insert u ( states2 d ) }

transit :: TransitionTable -> Maybe State -> Symbol -> Maybe State
transit     _        Nothing = const Nothing
transit table ( Just state ) = listToMaybe . NDFM.matchState states
  where
    stateMatchers :: TransitionTable
    stateMatchers = Map.filterWithKey ( \ k _ -> state == fst k ) table

    states :: [ ( NDFM.Matcher, State ) ]
    states = Map.toList $ Map.mapKeys snd stateMatchers

move :: DFM -> Symbol -> DFM
move dfm@DFM{..} symbol
  = dfm{ currentState = transit transitionsTable currentState symbol }

finite :: DFM -> Bool
finite DFM{..} = maybe False ( `elem` finiteStates ) currentState

fromString :: String -> DFM
fromString str = DFM [ last states ] ( head states ) table
  where
    states = undefined
    table = undefined

matchDFM :: DFM -> String -> Bool
matchDFM dfm = finite . foldl move dfm

match :: String -> String -> Bool
match regex = matchDFM ( fromString regex )
