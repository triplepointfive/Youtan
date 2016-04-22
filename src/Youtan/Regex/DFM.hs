{-# LANGUAGE RecordWildCards #-}

-- | Implementation of DFM.
module Youtan.Regex.DFM where

import Control.Arrow ( first, second )
import Control.Monad ( foldM )
import Data.List ( find, (\\), groupBy, intercalate )
import Data.Function ( on )
import Data.Maybe ( catMaybes, listToMaybe, fromMaybe )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State

import qualified Youtan.Regex.NDFM as NDFM ( State( .. ), NDFM( .. ), StateID )
import Youtan.Regex.Operators ( Counter(..), Operator(..), parseString, OperatorID )
import qualified Youtan.Regex.Operators as O ( initID, nextFreeID )
import Youtan.Regex.FM

type StateID = Char

initID :: StateID
initID = 'a'

nextFreeID :: StateID -> StateID
nextFreeID = succ

type DState = Set.Set NDFM.StateID

data DTable
  = DTable
  { table       :: !( Map.Map ( DState, Matcher ) ( Set.Set NDFM.StateID ) )
  , processed   :: !( Set.Set DState )
  , states2     :: !( Set.Set DState )
  , finishState :: !NDFM.StateID
  } deriving ( Eq, Show )

type TransitionTable = Map.Map StateID [ ( Matcher, StateID ) ]

data DFM = DFM
  { finiteStates     :: ![ StateID ]
  , startState       :: !StateID
  , transitionsTable :: !TransitionTable
  , transitions      :: !( Set.Set Matcher )
  , freeID           :: !StateID
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
    finish :: [ StateID ]
    finish = map snd $ Map.toList $ Map.filterWithKey ( \ k _ -> Set.member finishState k ) states

    states :: Map.Map ( Set.Set NDFM.StateID ) StateID
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

-- fromString :: String -> DFM
fromString str = undefined
  where
    rawTree, tree :: Operator
    rawTree = parseString str
    tree = evalState ( assignIDs rawTree ) O.initID

    nextID :: State OperatorID OperatorID
    nextID = modify O.nextFreeID >> get

    nullable :: Map.Map OperatorID Bool
    nullable = undefined

    assignIDs :: Operator -> State OperatorID Operator
    assignIDs ( Empty _ ) = Empty <$> nextID
    assignIDs ( Literal _ c ) = Literal <$> nextID <*> return c
    assignIDs ( Concatenation _ oper1 oper2 )
      = Concatenation <$> nextID <*> assignIDs oper1 <*> assignIDs oper2
    assignIDs ( Disjunction _ oper1 oper2 )
      = Disjunction <$> nextID <*> assignIDs oper1 <*> assignIDs oper2
    assignIDs ( Counts _ c oper )
      = Counts <$> nextID <*> return c <*> assignIDs oper
    assignIDs ( CharClass _ c ) = CharClass <$> nextID <*> return c
    assignIDs ( Group oper ) = Group <$> assignIDs oper

matchDFM :: DFM -> String -> Bool
matchDFM DFM{..} = finite . foldM move startState
  where
    finite :: Maybe StateID -> Bool
    finite = maybe False ( `elem` finiteStates )

    move :: StateID -> Symbol -> Maybe StateID
    move state = listToMaybe . matchState ( states state )
      where
        states :: StateID -> [ ( Matcher, StateID ) ]
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

type GroupID = StateID
type Groups = Set.Set ( Set.Set StateID )

group :: DFM -> DFM
group dfm@DFM{..} = buildFromGroups $ while (/=) split initGroups
  where
    initGroups = Set.fromList
      [ Set.fromList finiteStates
      , Set.fromList $ Map.keys transitionsTable \\ finiteStates
      ]

    groupMap :: Groups -> Map.Map StateID GroupID
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
        groupIDs :: Map.Map StateID GroupID
        groupIDs = groupMap splitted

        newState :: Set.Set StateID -> ( StateID, [ ( Matcher, StateID ) ] )
        newState set = ( groupIDs Map.! s, map ( second ( (Map.!) groupIDs ) ) tr )
          where s = head $ Set.toList set
                tr = ( transitionsTable Map.! s ) :: [ ( Matcher, StateID ) ]

    split :: Groups -> Groups
    split groups = Set.fromList
        $ map Set.fromList
        $ intercalate []
        $ map ( groupBy ( (==) `on` (Map.!) stateToGroups ) . Set.toList )
          ( Set.toList groups )
      where
        idToGroup :: Map.Map StateID GroupID
        idToGroup = groupMap groups

        stateToGroups :: Map.Map StateID ( Map.Map Matcher GroupID )
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

    uselessStates :: [ StateID ]
    uselessStates
      = Map.keys
      $ Map.filterWithKey
        ( \ k s -> ( k `notElem` finiteStates ) && all ( (==) k . snd ) s )
        transitionsTable

-- match :: String -> String -> Bool
-- match regex = matchDFM ( fromString regex )
