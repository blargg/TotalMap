{-# LANGUAGE Rank2Types #-}
module TotalMapSpec where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import qualified Data.Map as Map
import Data.TotalMap

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = fmap Map.fromList arbitrary
    shrink = map Map.fromList . shrink . Map.toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (TotalMap k a) where
    arbitrary =  TotalMap <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (TotalMap a b) where
    (=-=) = eq


totalMapFunctorSpec :: TestBatch
totalMapFunctorSpec = functor (pure (1, "a", 'a') :: TotalMap Int (Int, String, Char))

totalMapApplicativeSpec :: TestBatch
totalMapApplicativeSpec = applicative (pure (1, "a", 'a') :: TotalMap Int (Int, String, Char))

totalMapMonadSpec :: TestBatch
totalMapMonadSpec = monad (pure (1, "a", 'a') :: TotalMap Int (Int, String, Char))

natrualTransformToFunc :: TestBatch
natrualTransformToFunc = functorMorphism ((!) :: (forall a. TotalMap Int a -> (Int -> a)))

composeDoesThing :: (Arbitrary a, Arbitrary b, Show a, Show b, Show c, EqProp c, Ord a, Ord b) => TotalMap b c -> TotalMap a b -> Property
composeDoesThing x y = find (compose x y) =-= ((find x) . (find y))

testCompose :: TotalMap String Char -> TotalMap Int String -> Property
testCompose = composeDoesThing
