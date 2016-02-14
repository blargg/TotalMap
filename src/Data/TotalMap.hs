module Data.TotalMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Data.These (These (..))
import Data.Align (alignWith)

data TotalMap k a = TotalMap { otherwise :: a
                             , values :: Map k a
                             } deriving (Show,Eq)

instance Functor (TotalMap k) where
    fmap f (TotalMap x m) = TotalMap (f x) (fmap f m)

instance (Ord k) => Applicative (TotalMap k) where
    pure x = TotalMap x Map.empty
    (TotalMap f fs) <*> (TotalMap x xs) = TotalMap (f x) (alignWith combine fs xs)
        where combine (These g y) = g y
              combine (This g) = g x
              combine (That y) = f y

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

and :: TotalMap k Bool -> Bool
and (TotalMap b m) = Map.fold (&&) b m

or :: TotalMap k Bool -> Bool
or (TotalMap b m) = Map.fold (||) b m
