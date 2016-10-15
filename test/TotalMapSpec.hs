{-# LANGUAGE Rank2Types #-}
module TotalMapSpec where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import qualified Data.Map as Map
import Data.TotalMap
import Data.Universe

import Algebra.Lattice

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
composeDoesThing x y = find (compose x y) =-= (find x . find y)

testCompose :: TotalMap String Char -> TotalMap Int String -> Property
testCompose = composeDoesThing

foldJoinOverUniverse :: (Universe k, Ord k, JoinSemiLattice a, Eq a) => a -> TotalMap k a -> Bool
foldJoinOverUniverse x tm = foldJoinSemiLattice x tm == finalValue
    where finalValue = foldl (\/) x $ fmap (find tm) universe

testJoinBool :: Bool -> TotalMap Char Bool -> Bool
testJoinBool = foldJoinOverUniverse

foldMeetOverUniverse :: (Universe k, Ord k, MeetSemiLattice a, Eq a) => a -> TotalMap k a -> Bool
foldMeetOverUniverse x tm = foldMeetSemiLattice x tm == finalValue
    where finalValue = foldl (/\) x $ fmap (find tm) universe

testMeetBool :: Bool -> TotalMap Char Bool -> Bool
testMeetBool = foldMeetOverUniverse
