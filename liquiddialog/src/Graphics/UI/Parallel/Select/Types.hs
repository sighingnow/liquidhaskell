{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns      #-}

module Graphics.UI.Parallel.Select.Types where

import Graphics.UI.WX 

import qualified Data.List as L 


import Data.Maybe (catMaybes)
import Data.List (nub)
import Debug.Trace (trace) 

type FileName = String 


data Info src a i e = Info { fname     :: FileName
                         , solutions :: [Partition src a i e] 
                         , status    :: [(src, Status src a)]
                         }

data Partition src a i e
  = Partition { pId       :: Int
              , pColor    :: Color 
              , pContent  :: Zipper [Sol src a]
              , pWorkList :: WorkList i e [Sol src a]
              , pState    :: PState}

data PState = PWork | PDone | PInit 
  deriving Eq 

data WorkList i a b = WL {wl :: Pick i a, wcompare :: b -> b -> IO Compare, wio :: [(i,[a])] -> IO (Res b)}

emptyWL :: WorkList i a b -> Bool 
emptyWL = emptyPick . wl 

data Res a = ROK a | RNonLocal | RUnsafe 

data Compare = Better | Worse | Unrelated 
  deriving (Eq, Show)


getCurrent :: Int -> Zipper a -> a 
getCurrent i z = case getCurrent_maybe i z of 
  Just x -> x 
  Nothing -> error "getCurrent"


getCurrent_maybe :: Int -> Zipper a -> Maybe a 
getCurrent_maybe _ ZEmpty = Nothing
getCurrent_maybe i z = Just $ go i (zprev z ++ (zcurrent z:znext z))
  where 
    go _ [x] = x
    go 1 (x:_) = x 
    go i (_:xs) = go (i-1) xs 

hasPrev :: Int -> Zipper a -> Bool 
hasPrev i ZEmpty = False 
hasPrev i _ = 1 < i 

hasNext :: Int -> Zipper a -> Bool 
hasNext i ZEmpty = False 
hasNext i z = i < (1 + (zlength z))

nextNumber :: Int -> Partition src a i e -> Int 
nextNumber i z = (zlength (pContent z)) - i 

zlength ZEmpty = 0 
zlength (Zipper _ p n) = 1 + length p + length n


prevNumber :: Int -> Partition src a i e -> Int 
prevNumber i _ = i 

workPartition :: Partition src a i e -> IO (Partition src a i e)
workPartition p | pState p == PDone 
  = return p 
workPartition p 
  = do (w, wl') <- makeWork (pId p) (pWorkList p)
       putStrLn ("makeWork " ++ show (pId p))
       case w of 
        Nothing -> return (p {pState = PDone, pWorkList = wl'})
        Just Nothing  -> do 
          if null (pCurrent $ wl wl') then return (p {pState = PDone, pWorkList = wl'}) else return (p {pState = PWork, pWorkList = wl'})
        Just (Just x)  -> do 
          content <- return $ insert {- Cmp (wcompare $ pWorkList p) -} (pContent p) x 
          return (p {pState = if (null (pCurrent $ wl wl')) then PDone else PWork, pWorkList = wl', pContent = content})


insertCmp :: (a -> a -> IO Compare) -> Zipper a -> a -> IO (Zipper a)
insertCmp _ ZEmpty z = return $ insert ZEmpty z
insertCmp f zp@(Zipper x prev next) z = do 
  cx    <- (x,) <$> f x z 
  cprev <- zip prev <$> mapM (f x) prev 
  cnext <- zip next <$> mapM (f x) next 
  if null [() | (_,Worse) <- (cx:(cprev ++ cnext))]
    then return $ insert zp z 
    else return zp
{- 
  let prev' = fst <$> filter ((/= Better) . snd) cprev
  let cnext = cx:cnext ++ (if (null $ [() | (_,Worse) <- (cx:(cprev ++ cnext))]) then [(z,Unrelated)] else [])  
  case fst <$> filter ((/= Better).snd) cnext of 
    (y:ys) -> return $ Zipper y prev' ys 
    []     -> return $ fromList $ reverse prev'

-}


-- exist v. v || 0 > x 
-- forall v. 


makeWork :: Int -> WorkList i a b -> IO (Maybe (Maybe b), WorkList i a b)
makeWork = makeWork' 50

makeWork' :: Int -> Int -> WorkList i a b -> IO (Maybe (Maybe b), WorkList i a b)
makeWork' _ i w@(WL p _ _) |  null (pCurrent p) =
  return (Nothing, w)
makeWork' t i (WL p c f) = do 
  let !xp = getPick p
  let (x,p') = getPick p 
  w <- f x
  case w of 
    ROK y  ->  return (Just (Just y), WL {- (filterElems (not . (<== (head $ pCurrent p))) -} (p') c f) 
    _ -> if 0 < t then (makeWork' (t-1) i (WL p' c f)) 
                  else (return (Just Nothing, WL p' c f))


-- [0, 1, 0, 1] [0, 0, 1, 1]


data Status src a = SOK [src] a | SClash [(src, a)]


data SrcSpan = SrcSpan {ssfrom :: (Int, Int), ssto :: (Int, Int)}
 deriving (Eq)


data Sol src a = Sol {ssrc :: src, stg :: src, sval :: a}


instance ShowPP a => ShowPP (Sol src a) where
  showpp = showpp . sval 

-- instance (ShowPP a, ShowPP b) => ShowPP (a,b) where
--   showpp (x,y) = showpp x ++ "\n\n" ++ showpp y

mappendSt :: (Eq a, Eq src) => Status src a -> Status src a -> Status src a 
mappendSt (SOK ss1 s1) (SOK ss2 s2) 
	  | s1 == s2  = SOK (L.nub (ss1 ++ ss2)) s1
	  | otherwise = SClash (L.nub (zip ss1 (repeat s1)++ zip ss2 (repeat s2)))
mappendSt (SClash s1) (SClash s2)  = SClash $ L.nub (s1 ++ s2)
mappendSt (SOK ss1 s1) (SClash ss) = SClash $ L.nub (((,s1)<$> ss1) ++ ss)
mappendSt (SClash ss) (SOK ss1 s1) = SClash $ L.nub (((,s1)<$> ss1) ++ ss)


makeStatus :: (Eq a, Eq src) => [Int] -> [Partition src a i e] -> [(src,Status src a)]
makeStatus is ps = combine $ concat [combine (go' <$> curr) | Just curr <- zipWith getCurrent_maybe is (pContent <$> ps)]
  where
  	-- go (Partition _ _ z _ _) = combine (go' <$> zcurrent z)
  	go' (Sol src tg s) = (src, SOK [tg] s)


  	insert [] x s = [(x,s)]
  	insert ((y,sy):acc) x s 
  	  | y == x    = (y,mappendSt s sy):acc
  	  | otherwise =(y,sy):insert acc x s 
  	combine = foldl (\acc (x,s) -> insert acc x s) []

applyIP :: (Zipper [Sol src a] -> Zipper [Sol src a]) -> Int -> [Partition src a i e] -> [Partition src a i e]
applyIP f _ [] = []
applyIP f i ((Partition j c z wl s):ps)
  | i == j 
  = Partition j c (f z) wl s:applyIP f i ps 
  | otherwise
  = Partition j c z wl s: applyIP f i ps 

movePNext, movePPrev :: Int -> [Partition src a i e] -> [Partition src a i e]
movePNext = applyIP moveNext
movePPrev = applyIP movePrev


data Zipper a = ZEmpty | Zipper {zcurrent :: a, zprev :: [a], znext :: [a]}
-- p1 p2 p3 p4 {c} n1 n2 n3 n4 

insert :: Zipper a -> a -> Zipper a 
insert ZEmpty x = Zipper x [] []
insert z x = z {znext = (znext z) ++ [x]}


moveNext :: Zipper a -> Zipper a 
moveNext (Zipper x p (n:ns))
  = {- trace ("MOVE NEXT = " ++ show (length p, 1 + length ns)) $ -}  Zipper n (p ++ [x]) ns
moveNext z 
  = z 
{- 
moveNext (Zipper x [] [])
  = Zipper x [] []
moveNext (Zipper x (p:ps) [])
  = Zipper p [x] ps
moveNext ZEmpty
  = ZEmpty
-}

movePrev :: Zipper a -> Zipper a 

movePrev (Zipper x ps@(_:_) ns)
  = Zipper (last ps) (init ps) (x:ns)
movePrev z 
  = z 
{- 
movePrev (Zipper x [] [])
  = Zipper x [] []
movePrev (Zipper x [] ns)
  = Zipper (last ns) (init ns) [x]
movePrev ZEmpty
  = ZEmpty
-}

fromList :: [a] -> Zipper a 
fromList []     = ZEmpty
fromList (x:xs) = Zipper x [] xs 






makePInfo :: (Eq a, Eq src) => String -> [WorkList i e [Sol src a]] -> (Info src a i e)
makePInfo str wls =  Info str ps (makeStatus (repeat 1) ps) 
  where 
  	ps = zipWith3 f [1..] colors (filter (not . emptyWL) wls)
  	f i c wl = Partition i c ZEmpty wl PInit

colors :: [Color]
colors = concat $ repeat [red, blue, cyan, magenta, yellow] -- green



class HasSpan a where 
  hasSpan :: a -> ((Int, Int), (Int, Int))

instance HasSpan ((Int, Int), (Int, Int)) where
  hasSpan x = x 

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

instance ShowPP a => ShowPP [a] where
  showpp = unlines . map showpp 


getPick :: Pick i a -> ([(i, [a])], Pick i a)
getPick pk = (currentElem pk, makeNext pk)


initPick :: [(i,[a])] -> Pick i a
initPick xs 
  = Pick (sizeP (map snd xs)) 
         xs
         elems  
         False 
         (sizeP (map snd xs))
  where
    elems = incrBin (map (\(_,es) -> map (const 0) es) xs)

{- 
powersets :: [(i, [a])] ->  Pick i [a] 
powersets = traceShow "POWERSETS" . initPick . map (mapSnd powerset)

initPick :: [(i, [a])] -> Pick i a 
initPick xs 
  = Pick (map (length . snd) xs) xs (transverse (map (const 1) xs) (map (length . snd) xs)) 1 (length xs) False 
-}

traceShow str a = trace (str ++ show a) a 

mapSnd f (x, y) = (x, f y)

powerset :: [a] -> [[a]]
powerset xs 
  = []: concat (map (`go` xs) [1..length xs])
  where
    go 1 xs = map (:[]) xs 
    go i (x:xs) = (map (x:) (go (i-1) xs)) ++ go i xs
    go _ _ = []


emptyPick :: Pick i a -> Bool 
emptyPick = null . pElems 

type PIndex = [[Int]]
data Pick i a 
  = Pick { pMaxIdx  :: Int
         , pElems   :: [(i, [a])]
         , pCurrent :: [PIndex]
         , pDone    :: Bool 
         , pLeft    :: Int 
         } 


filterElems f p = p {pCurrent = filter f (pCurrent p)}
-- [0, 1, 0] <== [0, 1, 1] 

(<==) :: PIndex -> PIndex -> Bool 
idx1 <== idx2 = go idx1 idx2 
 where
  go ((1:xs):xss) ((y:ys):yss) | y /= 1 =  False  
  go ((_:xs):xss) ((_:ys):yss) = go (xs:xss) (ys:yss) 
  go ([]:xss) (_:yss) = go xss yss
  go [] _ = True 



incrBin :: PIndex -> [PIndex]
incrBin xs = take (sizeP xs) $ catMaybes $ concatMap ( `incrPS` xs) [0.. (sizeP xs)]

sizeP [] = 1
sizeP (x:xs) = (2^(length x))* sizeP xs 

incrPS :: Int -> PIndex -> [Maybe PIndex]
incrPS 0 xss      = [Just xss]  
incrPS _ []       = [Nothing]
incrPS m (xs:xss) = [Just (xs':xss') | i <- [0..m], Just xs'<- get i xs, Just xss' <- incrPS (m-i) xss]
  where
    get 0 xs     = [Just xs] 
    get i []     = [Nothing] 
    get i (x:xs) = map Just (((0:) <$> catMaybes (get i xs)) ++ ((1:) <$> (catMaybes (get (i-1) xs)))) 


instance Show (Pick i a) where
  show p = "PICK : NEXT = " ++ show (pMaxIdx p) ++ "\n CURRENT = " ++ if (null $ pCurrent p) then "NULL" else show (take 2 $ pCurrent p) 

{- 
transverse :: [Int] -> [Int] -> [[Int]]
transverse i m = go (sum i) [(zip i m)]
  where
    go i x | i == mm = nub (map (map fst) x)
    go i x = (nub (((map fst) <$> x))) ++ go (i+1) (concatMap incr x)
    mm = sum m 

incr :: [(Int, Int)] -> [[(Int, Int)]]
incr ((v, m):xss)
  | v < m = ((v+1,m):xss) : (map ((v,m):) (incr xss))
  | otherwise = map ((v,m):) (incr xss)
incr [] = []  

-}

currentElem :: Pick i a -> [(i, [a])]
currentElem p | null (pCurrent p) = []
currentElem p = zipWith f (pElems p) (head $ pCurrent p)
  where
    f (i,es) is = (i, fst <$>  filter ((==1) . snd) (zip es is))


makeNext :: Pick i a -> Pick i a 
makeNext p | null (pCurrent p) 
  = trace ("makeNext NIL") (p{pDone = True})
makeNext p | pLeft p <= 0 
  = error "makeNext"
makeNext p 
  = p{pCurrent = tail (pCurrent p), pLeft = pLeft p - 1}



