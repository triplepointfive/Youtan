{-# LANGUAGE RecordWildCards #-}

-- | Implementation of DFM.
module Youtan.Regex.DFM where

import Control.Arrow ( second )
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
import Youtan.Regex.OperatorsExtra ( assignIDs, simplifyCounter, operID )

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
  , startState2 :: !( Maybe DState )
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
buildDFM DTable{..} = DFM finish st finStates trans lastState
  where
    st = maybe initID ( \ x -> states Map.! x ) startState2

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
      Nothing

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

type Nullable = Map.Map OperatorID Bool
type Posable = Map.Map OperatorID ( Set.Set OperatorID )

type S = Set.Set OperatorID
type Table = Map.Map ( S, Matcher ) S

fromString :: String -> DFM
fromString str =
  -- error $
    -- show tree ++ "\n\n" ++
    -- show nullable ++ "\n\n" ++
    -- show firstPos ++ "\n\n" ++
    -- show lastPos ++ "\n\n" ++
    -- show followPos
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
    states = step ( [ initID ], [], Map.empty )
      where
        initID = firstPos Map.! operID tree

        step :: ( [ S ], [ S ], Table ) -> Table
        step (   [], _, tran ) = tran
        step ( t:xs, d, tran ) = step $ Set.foldl buildTrans ( xs, t : d, tran ) t
          where
            buildTrans :: ( [ S ], [ S ], Table ) -> OperatorID -> ( [ S ], [ S ], Table )
            buildTrans ( unmarked, marked, tran ) p =
              if Map.member p followPos then
                ( if ( u `notElem` unmarked ) && ( u `notElem` marked )
                     then u:unmarked
                     else unmarked
                , marked
                , Map.insert ( t, m ) u tran
                )
              else
                ( unmarked, marked, tran )
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

match :: String -> String -> Bool
match regex = matchDFM ( fromString regex )
