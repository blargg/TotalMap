module Main where

import Data.TotalMap

main :: IO ()
main = do
    -- lookat presents it as a list of the values for keys 0 through 10
    let lookat = lookAtThese [0..3]
    putStrLn $ "v1:                " ++ show (lookat v1)
    putStrLn $ "v2:                " ++ show (lookat v2)
    putStrLn $ "(+) <$> v1 <*> v2: " ++ show (lookat v3)

lookAtThese :: (Ord k) => [k] -> TotalMap k a -> [a]
lookAtThese ks m = fmap (m!) ks

-- Every Value is 1
v1 :: TotalMap Int Int
v1 = pure 1

-- Every Value is 0, except at indexes 1 to 10 are the squares of their index
v2 :: TotalMap Int Int
v2 = fromList 0 ((\x -> (x, x*x)) <$> [1..3])

v3 :: TotalMap Int Int
v3 = (+) <$> v1 <*> v2
