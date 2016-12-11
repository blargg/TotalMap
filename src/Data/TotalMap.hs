{-# LANGUAGE DeriveGeneric #-}
module Data.TotalMap( TotalMap()
                    , compose
                    , fromList
                    , (!)
                    , find
                    , lookup
                    , insert
                    , foldJoinSemiLattice
                    , foldMeetSemiLattice
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Data.These (These (..))
import Data.Align (alignWith)
import Data.Serialize
import Data.Universe
import GHC.Generics
import Algebra.Lattice

import Test.QuickCheck

data TotalMap k a = TotalMap { defaultValue :: a
                             , values :: Map k a
                             } deriving (Show,Eq,Generic)

instance (Ord k, Serialize k, Serialize a) => Serialize (TotalMap k a)

instance Functor (TotalMap k) where
    fmap f (TotalMap x m) = TotalMap (f x) (fmap f m)

instance (Ord k) => Applicative (TotalMap k) where
    pure x = TotalMap x Map.empty
    (TotalMap f fs) <*> (TotalMap x xs) = TotalMap (f x) (alignWith combine fs xs)
        where combine (These g y) = g y
              combine (This g) = g x
              combine (That y) = f y

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (TotalMap k a) where
    arbitrary =  TotalMap <$> arbitrary <*> arbitrary


join' :: (Ord k) => TotalMap k (TotalMap k a) -> TotalMap k a
join' (TotalMap (TotalMap d defs) ms) = TotalMap d m
    where m = Map.union diagonal defs
          diagonal = Map.mapWithKey lookup ms

instance (Ord k) => Monad (TotalMap k) where
    v >>= f = join' (fmap f v)

compose :: (Ord b) => TotalMap b c -> TotalMap a b -> TotalMap a c
compose f (TotalMap g gs) = TotalMap c cs
    where c = f ! g
          cs = (f !) <$> gs

fromList :: (Ord k) => a -> [(k, a)] -> TotalMap k a
fromList d vs = TotalMap d (Map.fromList vs)

infixl 9 !
(!) :: Ord k => TotalMap k a -> k -> a
TotalMap def m ! key = Map.findWithDefault def key m

find :: Ord k => TotalMap k a -> k -> a
find = (!)

lookup :: Ord k => k -> TotalMap k a -> a
lookup = flip (!)

insert :: Ord k => k -> a -> TotalMap k a -> TotalMap k a
insert key x (TotalMap def m) = TotalMap def (Map.insert key x m)

foldJoinSemiLattice :: (Universe k, Ord k, JoinSemiLattice a) => a -> TotalMap k a -> a
foldJoinSemiLattice = foldIdemOverKeys (\/) universe

foldMeetSemiLattice :: (Universe k, Ord k, MeetSemiLattice a) => a -> TotalMap k a -> a
foldMeetSemiLattice = foldIdemOverKeys (/\) universe

foldIdemOverKeys :: (Ord k) => (a -> a -> a) -> [k] -> a -> TotalMap k a -> a
foldIdemOverKeys _ [] x _ = x
foldIdemOverKeys f (k:ks) x tm@(TotalMap d m) = case Map.lookup k m of
    Just value -> foldIdemOverKeys f ks (f x value) tm'
    Nothing -> foldl f (f x d) m
    where tm' = TotalMap d (Map.delete k m)
