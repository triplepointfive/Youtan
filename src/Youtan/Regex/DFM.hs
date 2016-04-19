{-# LANGUAGE RecordWildCards #-}

-- | Implementation of DFM.
module Youtan.Regex.DFM where

import Control.Arrow ( second )
import Control.Monad ( foldM )
import Data.List ( find, (\\), groupBy )
import Data.Function ( on )
import Data.Maybe ( catMaybes, listToMaybe, fromMaybe )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Youtan.Regex.NDFM as NDFM ( State( .. ), NDFM( .. ), StateID )
import Youtan.Regex.FM

type State = Char

initID :: State
initID = 'a'

nextFreeID :: State -> State
nextFreeID = succ

type DState = Set.Set NDFM.StateID

data DTable
  = DTable
  { table       :: !( Map.Map ( DState, Matcher ) ( Set.Set NDFM.StateID ) )
  , processed   :: !( Set.Set DState )
  , states2     :: !( Set.Set DState )
  , finishState :: !NDFM.StateID
  } deriving ( Eq, Show )

type TransitionTable = Map.Map State [ ( Matcher, State ) ]

data DFM = DFM
  { finiteStates     :: ![ State ]
  , startState       :: !State
  , transitionsTable :: !TransitionTable
  , transitions      :: !( Set.Set Matcher )
  , lastID           :: !State
  } deriving Show

fromNDFM :: NDFM.NDFM -> DFM
fromNDFM = buildDFM . buildDTable

markState :: DTable -> ( DState, DTable )
markState dtable@DTable{..} = ( Set.elemAt 0 states2,
  dtable{ states2 = Set.deleteAt 0 states2,
          processed = Set.insert ( Set.elemAt 0 states2 ) processed } )

buildDFM :: DTable -> DFM
buildDFM DTable{..} = DFM finish initID finStates trans lastState
  where
    finish :: [ State ]
    finish = map snd $ Map.toList $ Map.filterWithKey ( \ k _ -> Set.member finishState k ) states

    states :: Map.Map ( Set.Set NDFM.StateID ) State
    states = Map.fromList $ zip ( Set.toList processed ) ( iterate nextFreeID initID )

    lastState = iterate nextFreeID initID !! Map.size states

    trans :: Set.Set Matcher
    trans = Set.fromList $ map snd $ Map.keys table

    finStates :: TransitionTable
    finStates = Map.foldlWithKey
      ( \ group ( from, m ) to -> Map.insertWith (++) ( states Map.! from ) [ ( m, states Map.! to ) ] group )
      Map.empty table

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

    move :: Matcher -> DState -> Set.Set NDFM.StateID
    move m =
        Set.map head
      . Set.filter ( not . null )
      . Set.map ( map snd . filter ( (==) m . fst ) . NDFM.branches . ( Map.! ) states )

    symbols :: DState -> Set.Set Matcher
    symbols = Set.foldl Set.union Set.empty .
      Set.map ( Set.fromList . map fst . NDFM.branches . ( Map.! ) states )

    step :: DTable -> DTable
    step dtable2@DTable{..}
      | Set.null states2 = dtable2
      | otherwise        = step ( foldl ( update state ) dTable ( symbols state ) )
      where
        ( state, dTable ) = markState dtable2

    update :: DState -> DTable -> Matcher -> DTable
    update state d a = up{ table = Map.insert ( state, a ) u ( table d ) }
      where
        u = eClosure ( move a state )
        up = if Set.member u ( processed d )
             then d
             else d{ states2 = Set.insert u ( states2 d ) }

fromString :: String -> DFM
fromString str = undefined

matchDFM :: DFM -> String -> Bool
matchDFM DFM{..} = finite . foldM move startState
  where
    finite :: Maybe State -> Bool
    finite = maybe False ( `elem` finiteStates )

    move :: State -> Symbol -> Maybe State
    move state = listToMaybe . matchState ( states state )
      where
        states :: State -> [ ( Matcher, State ) ]
        states = fromMaybe [] . flip Map.lookup transitionsTable

closureDFM :: DFM -> DFM
closureDFM dfm@DFM{..} =
    dfm{ transitionsTable = closuredTable
       , lastID = q
       }
  where
    q = nextFreeID lastID
    closureRow = map ( \x -> ( x, q ) ) trans
    trans = Set.toList transitions

    closuredTable :: TransitionTable
    closuredTable
      = Map.insert q closureRow
      $ Map.map (\ v -> map
        ( \m -> fromMaybe ( m, q ) ( find ( (==) m . fst ) v ) )
        trans )
        ( foldl
          ( \t s -> Map.insertWith ( const id ) s [] t )
          transitionsTable
          finiteStates )

newtype GroupID = GroupID State
  deriving ( Eq )

instance Show GroupID where
  show ( GroupID a ) = show a

type Groups = Set.Set ( Set.Set State )

group :: DFM -> DFM
group dfm@DFM{..} = undefined
  where
    initGroups = Set.fromList [ Set.fromList finiteStates,
                                Set.fromList $ Map.keys transitionsTable \\ finiteStates ]

    split :: Groups -> Groups
    split groups
      = if groups == splited then splited else split splited
      where
        stateIDs = Map.keys transitionsTable

        idToGroup :: Map.Map State GroupID
        idToGroup = snd $
          Set.foldl (\(lastID, pairs ) group ->
            ( nextFreeID lastID, Set.foldl (\l s -> Map.insert s ( GroupID lastID ) l ) pairs group ) )
          ( initID, Map.empty )
          groups

        stateToGroups :: Map.Map State ( Map.Map Matcher GroupID )
        stateToGroups = Map.map
          ( Map.fromList . map ( second ( ( Map.! ) idToGroup ) ) )
          transitionsTable

        splited :: Groups
        splited = Set.map Set.fromList $ Set.fromList $
          groupBy ( (==) `on` (Map.!) stateToGroups )
          stateIDs

minimize :: DFM -> DFM
minimize = undefined

match :: String -> String -> Bool
match regex = matchDFM ( fromString regex )
