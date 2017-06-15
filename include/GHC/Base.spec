module spec GHC.Base where

import GHC.CString
import GHC.Prim
import GHC.Classes
import GHC.Types




// measure null :: [a] -> Bool
// null []     = true 
// null (y:ys) = false

measure fst :: (a,b) -> a
fst (a,b) = a

measure snd :: (a,b) -> b
snd (a,b) = b

qualif Fst(v:a, y:b): (v = (fst y))
qualif Snd(v:a, y:b): (v = (snd y))

map       :: (a -> b) -> xs:[a] -> {v: [b] | len v == len xs}
(++)      :: xs:[a] -> ys:[a] -> {v:[a] | len v == len xs + len ys}

($)       :: (a -> b) -> a -> b
id        :: x:a -> {v:a | v = x}

data variance Text.ParserCombinators.ReadPrec.ReadPrec contravariant

