module Variations where
{-@ LIQUID "--gradual" @-}
{- LIQUID "--ginteractive" @-}

{-@ check :: Int -> Bool @-}
check :: Int -> Bool
check = undefined 

{-@ get :: {x:Int | x > 0} -> String @-}
get :: Int -> String
get = undefined 


-- does not type check
{-@ f :: {v:Int | ?? } -> String @-}
f :: Int -> String
f x = if check(x)
      then get(x)
      else get(-x)


-- does not type check
{-@ f2 :: {v:Int | ?? } -> {v:Bool | ?? } -> String @-}
f2 :: Int -> Bool -> String
f2 x y  = if y
          then get(x)
          else get(-x)