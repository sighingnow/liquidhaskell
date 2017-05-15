{-# LANGUAGE TupleSections #-}

module Graphics.UI.Select.Types where

import Graphics.UI.WX 

import qualified Data.List as L 

type FileName = String 


data Info src a = Info { fname     :: FileName
                       , solutions :: [Partition src a] 
                       , status    :: [(src, Status src a)]
                       }

data Partition src a = Partition Int Color (Zipper [Sol src a])

data Status src a = SOK [src] a | SClash [(src, a)]


data SrcSpan = SrcSpan {ssfrom :: (Int, Int), ssto :: (Int, Int)}
 deriving (Eq)


data Sol src a = Sol {ssrc :: src, stg :: src, sval :: a}




mappendSt :: (Eq a, Eq src) => Status src a -> Status src a -> Status src a 
mappendSt (SOK ss1 s1) (SOK ss2 s2) 
	  | s1 == s2  = SOK (L.nub (ss1 ++ ss2)) s1
	  | otherwise = SClash (L.nub (zip ss1 (repeat s1)++ zip ss2 (repeat s2)))
mappendSt (SClash s1) (SClash s2)  = SClash $ L.nub (s1 ++ s2)
mappendSt (SOK ss1 s1) (SClash ss) = SClash $ L.nub (((,s1)<$> ss1) ++ ss)
mappendSt (SClash ss) (SOK ss1 s1) = SClash $ L.nub (((,s1)<$> ss1) ++ ss)


makeStatus :: (Eq a, Eq src) => [Partition src a] -> [(src,Status src a)]
makeStatus ps = combine $ concat (go <$> ps)
  where
  	go (Partition _ _ z) = combine (go' <$> current z)
  	go' (Sol src tg s) = (src, SOK [tg] s)


  	insert [] x s = [(x,s)]
  	insert ((y,sy):acc) x s 
  	  | y == x    = (y,mappendSt s sy):acc
  	  | otherwise =(y,sy):insert acc x s 
  	combine = foldl (\acc (x,s) -> insert acc x s) []

applyIP :: (Zipper [Sol src a] -> Zipper [Sol src a]) -> Int -> [Partition src a] -> [Partition src a]
applyIP f _ [] = []
applyIP f i ((Partition j c z):ps)
  | i == j 
  = Partition j c (f z):applyIP f i ps 
  | otherwise
  = Partition j c z : applyIP f i ps 

movePNext, movePPrev :: Int -> [Partition src a] -> [Partition src a]
movePNext = applyIP moveNext
movePPrev = applyIP movePrev


data Zipper a = Zipper {current :: a, prev :: [a], next :: [a]}
-- p1 p2 p3 p4 {c} n1 n2 n3 n4 

moveNext :: Zipper a -> Zipper a 
moveNext (Zipper x [] [])
  = Zipper x [] []
moveNext (Zipper x p (n:ns))
  = Zipper n (p ++ [x]) ns
moveNext (Zipper x (p:ps) [])
  = Zipper p [x] ps

movePrev :: Zipper a -> Zipper a 
movePrev (Zipper x [] [])
  = Zipper x [] []
movePrev (Zipper x ps@(_:_) ns)
  = Zipper (last ps) (init ps) (x:ns)
movePrev (Zipper x [] ns)
  = Zipper (last ns) (init ns) [x]


fromList :: [a] -> Zipper a 
fromList (x:xs) = Zipper x [] xs 








makePInfo :: (Eq a, Eq src) => String -> [[[Sol src a]]] ->  (Info src a)
makePInfo str ss =  Info str ps (makeStatus ps)
  where 
  	ps = zipWith3 f [1..] colors ss
  	f i c ss = Partition i c (fromList ss)

colors :: [Color]
colors = concat $ repeat [red, green, blue, cyan, magenta, yellow]



class HasSpan a where 
  hasSpan :: a -> ((Int, Int), (Int, Int))


pointsSrc :: (HasSpan a) => a -> (Point, Point)
pointsSrc x = pointsSrcSpan (SrcSpan x1 x2)
  where
      (x1, x2) = hasSpan x 

moveUp :: (HasSpan a) => Int -> a -> SrcSpan 
moveUp i x  = moveUpSrc i (SrcSpan x1 x2) 
    where 
      (x1, x2) = hasSpan x 

pointsSrcSpan (SrcSpan (ln, cs) (_, cf))
     = (point (txtcolumn (cs-1)) (txtline ln), point (txtcolumn cf) (txtline ln))
moveUpSrc i (SrcSpan (sl, sc) (tl, tc))
    =  SrcSpan (sl-i, sc) (tl-i, tc)


instance HasSpan SrcSpan where
  hasSpan (SrcSpan x y) = (x, y)

heighttxt ln (cs, cf)  
  = (point (txtcolumn (cs-1)) (txtline ln), point (txtcolumn cf) (txtline ln))

txtline x   =  x * 13
txtcolumn x =  6 * x 


class ShowPP a where
	showpp :: a -> String 
