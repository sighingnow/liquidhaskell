-- | Sec 8 from Gradual Refinement Types 

module Measures where
{-@ LIQUID "--gradual" @-}
{-@ LIQUID "--scrape-used-imports" @-}


-- This does not work because I need the special locality treatment for measures
{-@ g :: x:{v:Int | 0 < v } -> y:{v:Int |  ?? }  -> Bool @-}
g :: Int -> Int -> Bool  
g x y = foo x y 

foo :: Int -> Int -> Bool  
{-@ foo :: x:Int -> {v:Int | 0 <= v && v < x} -> Bool @-}
foo _ _ = True   


{- 
-- This does not work because I need the special locality treatment for measures
{- f :: x:[a] -> {v:Int | true } -> a -> Bool @-}
f :: Eq a => [a] -> Int -> a -> Bool  
f xs i y= xs!!i == y 
-}
-- ?? = 0 <= v && v < len x

{- 
-- IS LOCAL 
exists v.  0 <= v && v < len x

ISSAT? 
forall v. v < 0 || app len x <= v 


-}
