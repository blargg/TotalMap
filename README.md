Total Map
=========

A map where a value is defined for every key in the key type.

Has intances for functor, applicative and monad. The functor instance is a natural transformation to the function monad, ```((->) r)```.
```
-- Every Value is 1
v1 :: TotalMap Int Int
v1 = pure 1

-- Every Value is 0, except at indexes 1 to 3 are the squares of their index
v2 :: TotalMap Int Int
v2 = fromList 0 ((\x -> (x, x*x)) <$> [1..3])

-- uses (+) to combine the values for matching keys
v3 :: TotalMap Int Int
v3 = (+) <$> v1 <*> v2
```
