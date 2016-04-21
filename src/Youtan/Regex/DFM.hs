{-# LANGUAGE RecordWildCards #-}

-- | Implementation of DFM.
module Youtan.Regex.DFM where

import Control.Arrow ( second )
import Control.Monad ( foldM )
import Data.List ( find, (\\), groupBy, intercalate )
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
  , freeID           :: !State
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
      ( Map.fromList ( map (\x -> (x, [])) finish ) ) table

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
       , freeID = nextFreeID freeID
       }
  where
    closureRow = map ( \x -> ( x, freeID ) ) trans
    trans = Set.toList transitions

    closuredTable :: TransitionTable
    closuredTable
      = Map.insert freeID closureRow
      $ Map.map (\ v -> map
        ( \m -> fromMaybe ( m, freeID ) ( find ( (==) m . fst ) v ) )
        trans )
        transitionsTable

type GroupID = State
type Groups = Set.Set ( Set.Set State )

group :: DFM -> DFM
group dfm@DFM{..} = buildFromGroups $ while (/=) split initGroups
  where
    initGroups = Set.fromList
      [ Set.fromList finiteStates
      , Set.fromList $ Map.keys transitionsTable \\ finiteStates
      ]

    groupMap :: Groups -> Map.Map State GroupID
    groupMap = snd .
      Set.foldl (\(lastID, pairs ) group ->
        ( nextFreeID lastID,
          Set.foldl
            (\l s -> Map.insert s lastID l )
            pairs group ) )
      ( initID, Map.empty )

    buildFromGroups :: Groups -> DFM
    buildFromGroups splitted = DFM
      { startState = groupIDs Map.!
          head ( Set.toList $ head $
                 Set.toList $ Set.filter ( startState `Set.member` ) splitted )
      , finiteStates = Set.toList
          $ Set.map ( \ l -> groupIDs Map.! head ( Set.toList l ) )
          $ Set.filter ( \l -> any ( `Set.member` l) finiteStates )
          splitted
      , freeID = iterate nextFreeID initID !! Set.size splitted
      , transitionsTable = Map.fromList $ map newState $ Set.toList splitted
      , transitions = transitions
      }
      where
        groupIDs :: Map.Map State GroupID
        groupIDs = groupMap splitted

        newState :: Set.Set State -> ( State, [ ( Matcher, State ) ] )
        newState set = ( groupIDs Map.! s, map ( second ( (Map.!) groupIDs ) ) tr )
          where s = head $ Set.toList set
                tr = ( transitionsTable Map.! s ) :: [ ( Matcher, State ) ]

    split :: Groups -> Groups
    split groups = Set.fromList
        $ map Set.fromList
        $ intercalate []
        $ map ( groupBy ( (==) `on` (Map.!) stateToGroups ) . Set.toList )
          ( Set.toList groups )
      where
        idToGroup :: Map.Map State GroupID
        idToGroup = groupMap groups

        stateToGroups :: Map.Map State ( Map.Map Matcher GroupID )
        stateToGroups = Map.map
          ( Map.fromList . map ( second ( ( Map.! ) idToGroup ) ) )
          transitionsTable

while :: ( a -> a -> Bool ) -> ( a -> a ) -> a -> a
while con f v = let step x y = if x `con` y then step ( f x ) x else x
                in step ( f v ) v

minimize :: DFM -> DFM
minimize = clean . group . closureDFM

clean :: DFM -> DFM
clean dfm@DFM{..} = dfm{ transitionsTable = withoutLinks }
  where
    withoutLinks = Map.map ( filter ( ( `notElem` uselessStates ) . snd ) ) withoutStates

    withoutStates :: TransitionTable
    withoutStates = foldl ( flip Map.delete ) transitionsTable uselessStates

    uselessStates :: [ State ]
    uselessStates
      = Map.keys
      $ Map.filterWithKey
        ( \ k s -> ( k `notElem` finiteStates ) && all ( (==) k . snd ) s )
        transitionsTable

match :: String -> String -> Bool
match regex = matchDFM ( fromString regex )
