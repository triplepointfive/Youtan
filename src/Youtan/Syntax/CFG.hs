{-# LANGUAGE GADTs #-}

module Youtan.Syntax.CFG
-- ( Grammar( .. )
-- )
  where

import Control.Monad ( mplus )

data Grammar a b where
  NullParser :: Grammar a b
  Check :: ( a -> Bool ) -> Grammar a a
  (:|) :: Grammar a b -> Grammar a b -> Grammar a b
  (:&) :: Grammar a b -> Grammar a c -> Grammar a ( b, c )
  Push :: a -> Grammar a b -> Grammar a b
  FMap :: Grammar a c -> ( c -> b ) -> Grammar a b

instance Functor ( Grammar s ) where
  fmap = flip FMap

tok :: Eq a => a -> Grammar a a
tok x = Check ( == x )

single :: ( a -> Bool ) -> Grammar a a
single = Check

term :: Eq a => a -> ( a -> b ) -> Grammar a b
term v = FMap ( tok v )

parse :: [ a ] ->  Grammar a b -> Maybe b
parse [ c ] ( Check cond ) = if cond c then Just c else Nothing
parse x ( FMap g f )       = f <$> parse x g
parse x ( Push c g )       = parse ( c : x ) g
parse x ( c1 :| c2 )       = parse x c1 `mplus` parse x c2
parse (x:xs) (g :& g')     =
  parse xs ( Push x g :& g') `mplus` ( (,) <$> parse [] g <*> parse (x:xs) g' )
parse _ _                  = Nothing
