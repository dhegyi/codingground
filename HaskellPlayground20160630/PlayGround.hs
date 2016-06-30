-- playGround.hs
module PlayGround where

import Prelude hiding (map)

minList :: [Int] -> Int
minList []     = maxBound
minList (x:xs) = x `min` minList xs

map :: (a -> b) -> [a] -> [b]
map f []    = []
map f (x:xs)  = addToList (f x) (map f xs)
  where
    addToList :: a -> [a] -> [a]
    addToList x [] = [x]
    addToList x xs = x:xs


--retVal = "Bonjour, world!" ++ show a
retVal = show (map (\p -> p * p) [2,7,3])