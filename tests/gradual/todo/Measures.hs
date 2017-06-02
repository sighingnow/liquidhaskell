-- | Sec 8 from Gradual Refinement Types 

module Measures where
{-@ LIQUID "--gradual" @-}
{-@ LIQUID "--scrape-used-imports" @-}

{-@ g :: x:{[Int] | ??} -> y:{[Int] | ?? } -> Int @-}
g :: [Int] -> [Int] -> Int  
g xs ys = sumInt (mapInt (\i -> (xs!!i + ys!!i)) (range 0 (length xs)))

sumInt :: [Int] -> Int 
sumInt = sum 

mapInt :: (Int -> Int) -> [Int] -> [Int] 
mapInt = map 

{-@ range :: lo:Int -> hi:Int -> [{v:Int | 0 <= v && v < hi}] @-} 
range :: Int -> Int -> [Int] 
range lo hi
  | lo < hi = lo:range (lo+1) hi
  | otherwise = []


