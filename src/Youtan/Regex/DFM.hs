{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | Implementation of DFM.
module Youtan.Regex.DFM
( DFM
, DState
, DTable
, FM( .. )
, StateID
, fromNDFM
, fromString
, longestMatch
, longestMatchDFM
, match
, minimize
, squeeze
) where

import Control.Arrow ( first, second )
import Control.Monad.State ( State, evalState, get, modify )
import Control.Monad ( forM_, when, foldM, unless )
import Data.List ( find, groupBy, intercalate )
import Data.Function ( on )
import Data.Maybe ( catMaybes, listToMaybe, fromMaybe, mapMaybe )
import Data.String ( IsString( .. ) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Youtan.Regex.NDFM as NDFM ( State( .. ), NDFM( .. ) )
import Youtan.Regex.Operators ( Counter(..), Operator(..), parseString, OperatorID )
import qualified Youtan.Regex.Operators as O ( initID, nextFreeID )
import Youtan.Regex.FM
import Youtan.Regex.OperatorsExtra ( assignIDs, simplifyCounter, operID )
import Youtan.Utils ( while )

-- | Each 'DState' means a set of 'NDFM.StateID's.
type DState = Set.Set StateID

type DFMID  = Int

-- | Intermediate structure, could be converted to 'DFM'.
data DTable
  = DTable
  { table         :: !( Map.Map ( DState, Matcher ) DState )
  , processed     :: !( Set.Set DState )
  , states2       :: !( Set.Set DState )
  , completeState :: !StateID
  , startState2   :: !( Maybe DState )
  } deriving ( Eq, Show )

-- | Shortcut. TODO: Rewrite to map with matcher to plain 'StateID'.
type TransitionTable = Map.Map StateID [ ( Matcher, StateID ) ]

-- | Represents deterministic finite automaton.
data DFM 
  = DFM
  { finiteStates     :: !( Set.Set StateID )
  , startState       :: !StateID
  , transitionsTable :: !TransitionTable
  , transitions      :: !( Set.Set Matcher )
  , freeID           :: !StateID
  , finishIDs        :: !( Map.Map StateID DFMID )
  } deriving ( Eq, Show )

type StateMapping = ( StateID, Map.Map StateID StateID, Set.Set StateID )

instance Monoid DFM where
  mempty  = DFM
    { finiteStates     = Set.empty
    , startState       = initID
    , transitionsTable = Map.singleton initID []
    , transitions      = Set.empty
    , freeID           = nextFreeID initID
    , finishIDs        = Map.empty
    }

  mconcat = foldl mappend mempty

  mappend dfm1 dfm2 = 
    DFM
    { finiteStates     = finiteStates dfm1 `Set.union` dfm2UpdatedFinish
    , startState       = startState dfm1
    , transitionsTable = mergedTable
    , transitions      = Set.union ( transitions dfm1 ) ( transitions dfm2 )
    , freeID           = lastID
    , finishIDs        = finishIDs dfm1 `Map.union` newFinishIDs
    }
    where
      newFinishID = maximum ( 0 : map succ ( Map.elems ( finishIDs dfm1 ) ) )

      dfm2UpdatedFinish = Set.map ( statesMapping Map.! )
                        $ Set.filter ( `Map.member` statesMapping ) ( finiteStates dfm2 )

      newFinishIDs = Map.fromList $ map (, newFinishID ) $ Set.toList dfm2UpdatedFinish

      initMap = Map.singleton ( startState dfm2 ) ( startState dfm1 )

      mergedTable = Map.foldlWithKey
        unite
        ( transitionsTable dfm1 )
        accessibleDFM2States

      accessibleDFM2States = Map.filterWithKey
        ( \ k _ -> Map.member k statesMapping )
        ( transitionsTable dfm2 )

      unite table dfm2StateID matchers = Map.insertWith
        -- TODO: Use Map.merge or another way to check keys uniqueness.
        ( ++ )
        ( statesMapping Map.! dfm2StateID )
        ( map ( second ( statesMapping Map.! ) ) matchers )
        table

      ( lastID, statesMapping, _ ) = evalState ( transit ( startState dfm2 ) >> get )
        ( freeID dfm1, initMap, Set.empty )

      nextID :: State StateMapping StateID
      nextID = do
        ( next, _, _ ) <- get
        modify ( \ ( f, s, t ) -> ( nextFreeID f, s, t ) )
        return next

      addMatch :: StateID -> StateID -> State StateMapping ()
      addMatch dfm2NodeID dfm1NodeID =
        modify ( \ ( f, s, t ) -> ( f, Map.insert dfm2NodeID dfm1NodeID s, t ) )

      markState :: StateID -> State StateMapping ()
      markState dfm2NodeID =
        modify ( \ ( f, s, t ) -> ( f, s, Set.insert dfm2NodeID t ) )

      marked :: StateID -> State StateMapping Bool
      marked dfm2NodeID = do
        ( _, _, marks ) <- get
        return ( Set.member dfm2NodeID marks )

      transit :: StateID -> State StateMapping ()
      transit dfm2NodeID = do
        processed <- marked dfm2NodeID
        unless processed $ do
          markState dfm2NodeID
          ( _, mapping, _ ) <- get
          if dfm2NodeID `Map.member` mapping
          then
            let dfm1NodeID       = mapping Map.! dfm2NodeID
                dfm1NodeMatchers = ( transitionsTable dfm1 Map.! dfm1NodeID )
                sameMatchers     :: [ ( StateID, StateID ) ]
                sameMatchers     = mapMaybe ( \ ( m, s2 ) ->
                  ( ( s2,) . snd ) <$> find ( ( == ) m . fst ) dfm1NodeMatchers )
                  dfm2NodeMatchers
            in mapM_ ( uncurry addMatch ) sameMatchers
          else do
            newID <- nextID
            addMatch dfm2NodeID newID
          mapM_ ( transit . snd ) dfm2NodeMatchers
        where
          dfm2NodeMatchers = transitionsTable dfm2 Map.! dfm2NodeID

-- | Builds 'DFM' from an input regex.
instance IsString DFM where
  -- TODO: Sometimes creates extra inacsessible nodes
  fromString str =
    buildDFM $ DTable
      states
      ( Set.fromList $ Set.singleton O.initID : map fst ( Map.keys states )  )
      Set.empty
      O.initID
      ( Just ( firstPos Map.! operID tree ) )
    where
      rawTree :: Operator
      rawTree = simplifyCounter $ parseString str

      tree :: Operator
      tree = Concatenation ( O.nextFreeID O.initID )
        ( evalState ( assignIDs rawTree ) ( O.nextFreeID O.initID ) )
        ( Literal O.initID '#' )

      operators :: Map.Map OperatorID Matcher
      operators = evalState ( step tree >> get ) Map.empty
        where
          add :: Matcher -> OperatorID -> State ( Map.Map OperatorID Matcher ) ()
          add m o = modify ( Map.insert o m )

          step :: Operator -> State ( Map.Map OperatorID Matcher ) ()
          step ( Empty _ ) = return ()
          step ( Literal i l ) = add ( Exact l ) i
          step ( Disjunction _ oper1 oper2 ) =
            step oper1 >> step oper2
          step ( Concatenation _ oper1 oper2 ) =
            step oper1 >> step oper2
          step ( Counts _ _ oper ) = step oper
          step ( CharClass i c ) = add ( Class c ) i
          step ( Group oper ) = step oper

      states :: Table
      states = step ( [ firstPos Map.! operID tree ], [], Map.empty )
        where
          step :: ( [ S ], [ S ], Table ) -> Table
          step (   [], _, tran ) = tran
          step ( t:xs, d, tran ) = step ( rs, marked, tr )
            where
              marked = t : d
              ( rs, tr ) = evalState ( forM_ ( Set.toList t ) buildTrans >> get ) ( xs, tran )

              buildTrans :: OperatorID -> State ( [ S ], Table ) ()
              buildTrans p =
                when ( Map.member p followPos ) $ do
                  ( unmarked, _ ) <- get
                  when ( ( u `notElem` unmarked ) && ( u `notElem` marked ) ) $
                    modify ( first ( u : ) )
                  modify ( second ( Map.insert ( t, m ) u ) )
                where
                  m = operators Map.! p
                  u = followPos Map.! p

      set :: Ord o => o -> a -> State ( Map.Map o a ) a
      set operatorID val = modify ( Map.insert operatorID val ) >> return val

      nullable :: Nullable
      nullable = evalState ( step tree >> get ) Map.empty
        where
          step :: Operator -> State Nullable Bool
          step ( Empty i ) = set i True
          step ( Literal i _ ) = set i False
          step ( Disjunction i oper1 oper2 )
            = (||) <$> step oper1 <*> step oper2 >>= set i
          step ( Concatenation i oper1 oper2 )
            = (&&) <$> step oper1 <*> step oper2 >>= set i
          step ( Counts i c oper ) = case c of
            KleeneStar -> step oper >> set i True
            OneOrMore -> step oper >>= set i
            ZeroOrOne -> step oper >> set i True
          step ( CharClass i _ ) = set i False
          step ( Group oper ) = step oper

      firstPos :: Posable
      firstPos = evalState ( step tree >> get ) Map.empty
        where
          step :: Operator -> State Posable ( Set.Set OperatorID )
          step ( Empty i ) = set i Set.empty
          step ( Literal i _ ) = set i ( Set.singleton i )
          step ( Disjunction i oper1 oper2 ) =
            ( Set.union <$> step oper1 <*> step oper2 ) >>= set i
          step ( Concatenation i oper1 oper2 )
            | nullable Map.! operID oper1 =
              ( Set.union <$> step oper1 <*> step oper2 ) >>= set i
            | otherwise = step oper2 >> step oper1 >>= set i
          step ( Counts i _ oper ) = step oper >>= set i
          step ( CharClass i _ ) = set i ( Set.singleton i )
          step ( Group oper ) = step oper

      lastPos :: Posable
      lastPos = evalState ( step tree >> get ) Map.empty
        where
          step :: Operator -> State Posable ( Set.Set OperatorID )
          step ( Empty i ) = set i Set.empty
          step ( Literal i _ ) = set i ( Set.singleton i )
          step ( Disjunction i oper1 oper2 ) =
            ( Set.union <$> step oper1 <*> step oper2 ) >>= set i
          step ( Concatenation i oper1 oper2 )
            | nullable Map.! operID oper2 =
              ( Set.union <$> step oper1 <*> step oper2 ) >>= set i
            | otherwise = step oper1 >> step oper2 >>= set i
          step ( Counts i _ oper ) = step oper >>= set i
          step ( CharClass i _ ) = set i ( Set.singleton i )
          step ( Group oper ) = step oper

      followPos :: Posable
      followPos = evalState ( step tree >> get ) Map.empty
        where
          add :: Set.Set OperatorID -> OperatorID -> State Posable ()
          add ids operatorID = modify
            ( Map.alter ( Just . maybe ids ( Set.union ids ) ) operatorID )

          step :: Operator -> State Posable ()
          step ( Empty _ ) = return ()
          step ( Literal _ _ ) = return ()
          step ( Disjunction _ oper1 oper2 ) = step oper1 >> step oper2
          step ( Concatenation _ oper1 oper2 ) =
            let a = operID oper1; b = operID oper2
            in step oper1 >> step oper2 >>
              forM_ ( lastPos Map.! a ) ( add ( firstPos Map.! b ) )
          step ( Counts _ _ oper ) = let a = operID oper in
            step oper >> forM_ ( lastPos Map.! a ) ( add ( firstPos Map.! a ) )
          step ( CharClass _ _ ) = return ()
          step ( Group oper ) = step oper

instance FM DFM where
  -- | Checks whether 'DFM' accepts given string.
  matchFM DFM{..} = finite . foldM move startState
    where
      finite :: Maybe StateID -> Bool
      finite = maybe False ( `elem` finiteStates )

      move :: StateID -> Symbol -> Maybe StateID
      move stateID = listToMaybe . matchState ( states stateID )
        where
          states :: StateID -> [ ( Matcher, StateID ) ]
          states = fromMaybe [] . flip Map.lookup transitionsTable

-- | Converts 'NDFM.NDFM' into 'DFM'.
fromNDFM :: NDFM.NDFM -> DFM
fromNDFM = buildDFM . buildDTable

-- | Builds 'DFM' from 'DTable'.
buildDFM :: DTable -> DFM
buildDFM DTable{..} =
  DFM
  { finiteStates     = Set.fromList finish
  , startState       = st
  , transitionsTable = finStates
  , transitions      = trans
  , freeID           = lastState
  , finishIDs        = Map.fromList ( zip finish ( repeat 0 )  )
  }
  where
    st = maybe initID ( states Map.! ) startState2

    finish :: [ StateID ]
    finish = Map.elems $ Map.filterWithKey ( \ k _ -> Set.member completeState k ) states

    states :: Map.Map DState StateID
    states = Map.fromList $ zip ( Set.toList processed ) ( iterate nextFreeID initID )

    lastState = iterate nextFreeID initID !! Map.size states

    trans :: Set.Set Matcher
    trans = Set.fromList $ map snd $ Map.keys table

    finStates :: TransitionTable
    finStates = Map.foldlWithKey
      ( \ group ( from, m ) to -> Map.insertWith (++) ( states Map.! from ) [ ( m, states Map.! to ) ] group )
      ( Map.fromList ( map (\x -> (x, [])) finish ) ) table

-- | Converts 'NDFM.NDFM' into 'DTable'.
buildDTable :: NDFM.NDFM -> DTable
buildDTable NDFM.NDFM{..} = step newDTable
  where
    newDTable = DTable
      { table         = Map.empty
      , processed     = Set.empty
      , states2       = Set.singleton $ eClosure ( Set.singleton startState )
      , completeState = finishState
      , startState2   = Nothing -- Just ( Set.singleton startState )
      }

    eClosure :: DState -> DState
    eClosure = Set.foldl Set.union Set.empty . Set.map closure
      where
        closure :: StateID -> DState
        closure stateID = Set.insert stateID
          $ eClosure $ Set.fromList $ catMaybes
          [ NDFM.emptyBranch1 ndfmState, NDFM.emptyBranch2 ndfmState ]
          where ndfmState = states Map.! stateID

    move :: Matcher -> DState -> DState
    move m = Set.map head
           . Set.filter ( not . null )
           . Set.map ( map snd
                     . filter ( (==) m . fst )
                     . NDFM.branches
                     . ( Map.! ) states
                     )

    symbols :: DState -> Set.Set Matcher
    symbols = Set.foldl Set.union Set.empty .
      Set.map ( Set.fromList . map fst . NDFM.branches . ( Map.! ) states )

    markState :: DTable -> ( DState, DTable )
    markState dtable@DTable{..} = ( Set.elemAt 0 states2,
      dtable{ states2 = Set.deleteAt 0 states2,
              processed = Set.insert ( Set.elemAt 0 states2 ) processed } )

    step :: DTable -> DTable
    step dtable2@DTable{..}
      | Set.null states2 = dtable2
      | otherwise = step ( foldl ( update dState ) dTable ( symbols dState ) )
      where
        ( dState, dTable ) = markState dtable2

    update :: DState -> DTable -> Matcher -> DTable
    update dState d a = up{ table = Map.insert ( dState, a ) u ( table d ) }
      where
        u = eClosure ( move a dState )
        up = if Set.member u ( processed d )
             then d
             else d{ states2 = Set.insert u ( states2 d ) }

-- | Shortcut for nullable function.
type Nullable = Map.Map OperatorID Bool

-- | Shortcut for few functions.
type Posable = Map.Map OperatorID ( Set.Set OperatorID )

-- | TODO: Name in wisely.
type S = Set.Set OperatorID

-- | Table of transitions of original regex.
type Table = Map.Map ( S, Matcher ) S

-- | Ensures 'DFM' has a transition for each and every possible matcher for
-- all states.
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

-- | To simplify understanding of function types.
type GroupID = StateID

-- | Each group is a set of ids.
type Groups = Set.Set ( Set.Set StateID )

-- | Tries to remove excess nodes in a 'DFM' keeping the input language.
squeeze :: DFM -> DFM
squeeze dfm@DFM{..} = buildFromGroups $ while (/=) split initGroups
  where
    initGroups = Set.fromList
      [ finiteStates
      , Set.fromList ( Map.keys transitionsTable ) Set.\\ finiteStates
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
    buildFromGroups splitted = dfm
      { startState = groupIDs Map.!
          head ( Set.toList $ head $
                 Set.toList $ Set.filter ( startState `Set.member` ) splitted )
      , finiteStates = Set.map ( \ l -> groupIDs Map.! head ( Set.toList l ) )
          $ Set.filter ( \l -> any ( `Set.member` l) finiteStates )
          splitted
      , freeID = iterate nextFreeID initID !! Set.size splitted
      , transitionsTable = Map.fromList $ map newState $ Set.toList splitted
      }
      where
        groupIDs :: Map.Map StateID GroupID
        groupIDs = groupMap splitted

        newState :: Set.Set StateID -> ( StateID, [ ( Matcher, StateID ) ] )
        newState set =
            ( groupIDs Map.! s, map ( second ( (Map.!) groupIDs ) ) tr )
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

-- | Builds the minimal possible DFM from the given.
minimize :: DFM -> DFM
minimize = clean . squeeze . closureDFM

-- | Removes extra nodes, which lead to nowhere. Actually, this converts
-- 'DFM' into 'NDFM.NDFM', but who cares anyway.
clean :: DFM -> DFM
clean dfm@DFM{..} = dfm{ transitionsTable = withoutLinks }
  where
    withoutLinks = Map.map ( filter ( ( `notElem` uselessStates ) . snd ) )
      withoutStates

    withoutStates :: TransitionTable
    withoutStates = foldl ( flip Map.delete ) transitionsTable uselessStates

    uselessStates :: [ StateID ]
    uselessStates
      = Map.keys
      $ Map.filterWithKey
        ( \ k s -> ( k `notElem` finiteStates ) && all ( (==) k . snd ) s )
        transitionsTable

-- | Tries to apply regex to an input string.
match :: String -> Input -> Bool
match regex = matchFM ( fromString regex :: DFM )

-- | Returns the length of the longest matching substring of the input starting
-- from the beginning of string. Returns 'Nothing' if no matches.
-- Note: Actually, this is not deterministic - that version is prepared for
-- multiple matches per unit of input.
longestMatch :: String -> Input -> Maybe ( Int, DFMID )
longestMatch regex = longestMatchDFM ( fromString regex )

-- | See 'longestMatch'.
longestMatchDFM :: DFM -> Input -> Maybe ( Int, DFMID )
longestMatchDFM DFM{..} str = evalState ( move 0 str startState >> get ) Nothing
  where
    upd :: ( Int, DFMID ) -> Maybe ( Int, DFMID ) -> Maybe ( Int, DFMID )
    upd new Nothing = Just new
    upd new@( n, dfmID ) prev@( Just ( prevLenght, prevDfmID ) )
      | n > prevLenght                       = Just new
      | n == prevLenght && dfmID < prevDfmID = Just new
      | otherwise                            = prev

    -- TODO: Move to top level, remove duplication with 'matchFM'.
    finite :: StateID -> Bool
    finite x = x `elem` finiteStates 

    move :: Int -> String -> StateID -> State ( Maybe ( Int, DFMID ) ) ()
    move curPos input stateID = do
      when ( finite stateID ) ( modify ( upd ( curPos, finishIDs Map.! stateID ) ) )
      case input of
        [] -> return ()
        ( x : xs ) -> mapM_ ( move ( succ curPos ) xs ) ( matchState ( states stateID ) x  )
      where
        states :: StateID -> [ ( Matcher, StateID ) ]
        states = fromMaybe [] . flip Map.lookup transitionsTable
