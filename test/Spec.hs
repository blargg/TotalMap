import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import TotalMapSpec

main :: IO ()
main = do
    quickCheck testJoinBool
    quickCheck testMeetBool
    quickCheck testCompose
    quickBatch natrualTransformToFunc
    quickBatch totalMapFunctorSpec
    quickBatch totalMapApplicativeSpec
    quickBatch totalMapMonadSpec
