-- | Extra functions to help DFM deal with operators tree.
module Youtan.Regex.OperatorsExtra where

import Control.Monad.State

import Youtan.Regex.Operators ( Counter(..), Operator(..), OperatorID, initID, nextFreeID )

-- | Replaces counters in a tree with 'KleeneStar' keeping the input language.
simplifyCounter :: Operator -> Operator
simplifyCounter ( Disjunction i oper1 oper2 )
  = Disjunction i ( simplifyCounter oper1 ) ( simplifyCounter oper2 )
simplifyCounter ( Concatenation i oper1 oper2 )
  = Concatenation i ( simplifyCounter oper1 ) ( simplifyCounter oper2 )
simplifyCounter ( Counts i c oper ) = case c of
    KleeneStar -> Counts i c o
    OneOrMore -> Concatenation initID o ( Counts i KleeneStar o )
    ZeroOrOne -> Disjunction i o ( Empty initID )
  where
    o = simplifyCounter oper
simplifyCounter ( Group oper ) = Group ( simplifyCounter oper )
simplifyCounter oper = oper

-- | Assigns id to each and every single node (except for 'Group') of a tree.
assignIDs :: Operator -> State OperatorID Operator
assignIDs o = case o of
  Empty _ -> Empty <$> nextID
  Literal _ c -> Literal <$> nextID <*> return c
  Concatenation _ oper1 oper2 ->
    Concatenation <$> nextID <*> assignIDs oper1 <*> assignIDs oper2
  Disjunction _ oper1 oper2 ->
    Disjunction <$> nextID <*> assignIDs oper1 <*> assignIDs oper2
  Counts _ c oper ->
    Counts <$> nextID <*> return c <*> assignIDs oper
  CharClass _ c -> CharClass <$> nextID <*> return c
  Group oper -> Group <$> assignIDs oper
  where
    nextID :: State OperatorID OperatorID
    nextID = modify nextFreeID >> get

-- TODO: Replace me with data fields.
-- | Returns id of operator.
operID :: Operator -> OperatorID
operID ( Empty i ) = i
operID ( Literal i _ ) = i
operID ( Disjunction i _ _ ) = i
operID ( Concatenation i _ _ ) = i
operID ( Counts i _ _ ) = i
operID ( CharClass i _ ) = i
operID ( Group oper ) = operID oper
