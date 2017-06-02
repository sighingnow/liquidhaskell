{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.Parallel.Select.DrawSolutions where
import Graphics.UI.WX

import Graphics.UI.WXCore.WxcClassesAL

import Graphics.UI.Parallel.Select.Types

import Control.Concurrent

import qualified Data.List as L 
import Control.Monad (filterM)

import Data.Maybe (fromMaybe)

compute1 :: Var [Int] -> Var (Partition src b i e) -> IO ()
compute1 vis v = do 
  p <- varGet v 
  if pState p == PDone 
    then varUpdate vis (filter (/= (pId p))) >> putStrLn ("Done with " ++ show (pId p)) >> return ()
    else do 
      putStrLn ("call workPartition for " ++ show (pId p))
      p' <- workPartition p 
      varSet v p'

compute :: Var (Partition src b i e) -> IO ()
compute v = do 
  p <- varGet v 
  p' <- workPartition p 
  varSet v p'  
  if pState p' == PDone 
    then return ()
    else compute v


prepeatM_ :: Var Bool  -> Var [Int] -> (Var (Partition src b i e) -> IO ()) -> [Var (Partition src b i e)] -> IO ()
prepeatM_ vb vis f xs = do 
  b  <- varGet vb 
  is <- varGet vis 
  putStrLn ("IS = " ++ show is)
  if (not b && not (null is))then mapM_ f (map (\i -> xs!!(i-1)) is) >> prepeatM_ vb vis f xs
    else  (putStrLn "EXIT LOOP" >> return ())

{- 
chooseElements :: (ShowPP i, ShowPP e) => String -> Partition src b i [e] -> IO (Partition src b i [e])
chooseElements fn p = makePick fn (wl $ pWorkList p) >>=  (\pk -> return $ p{pWorkList = (pWorkList p){wl = pk}})

makePick :: (ShowPP i, ShowPP a) => String -> Pick i [a] -> IO (Pick i [a])
makePick fn pk = 
  powersets <$> mapM (\(i, es) -> ((i,)) <$> chooseElems fn i (head <$> es)) (pElems pk) 

chooseElems ::(ShowPP i, ShowPP a) => String -> i -> [a] -> IO [a] 
chooseElems fn i xs = {- (start $ do 

       -- create a non-user-resizable top-level (orphan) frame.
       f <- frame    [text := "Q Selection"]

       fc <- readFile fn
       p <- panel f [on paint := (\_ _ -> return ())]

       t <- timer f [interval := 20] 

       box <- checkBox f [text := "Foo"] 

       set p [font := fontFixed 
             , layout := column 80 [ floatLeft (label fc), widget box]
             ]


       ) >> -} filterM (chooseElem i) xs 

chooseElem ::(ShowPP i, ShowPP a) =>  i -> a -> IO Bool 
chooseElem i a  = do 
  putStrLn ("CHOOSING ON " ++ showpp i)
  putStrLn ("KEEP " ++ showpp a ++ " ? [y/n]")
  c <- getLine 
  if (not (null c)) && (head c == 'y' || head c == 'Y') then 
    (putStrLn "KEPT" >> return True)
    else (putStrLn ("THROWN WITH " ++ show c) >> return False)
-}

drawSolutions :: (ShowPP v, Eq v, Eq src, HasSpan src, ShowPP i, ShowPP e, ShowPP src) 
  => Info src v i e -> IO ()
drawSolutions info@(Info filename ss status) 
--   = start $ do 
  = start (drawSolutions' info)

drawSolutions' (Info filename ss status) =  do 
       fc <- readFile filename

       vsols  <- mapM varCreate ss
       vidxs  <- mapM varCreate (map (const 1) ss)
       vstats <- varCreate status 

       vstop <- varCreate False 
       vbsols <- varCreate [] 

       f <- frame    [text := "Interactive Inference"]


       p <- panel f [on paint := paintInfo f vsols vstats vbsols vidxs]

       t <- timer f [interval := 20] 
       vis <- varCreate [] -- [1 .. length vsols] 

       cbots <- mapM (chooseButtons f p) (zip ss vsols)
       sbots <- mapM (showButtons f p) (zip vsols vidxs)
       sibots <- map widget <$> (mapM (\i -> button f [text := ("Solve " ++ show i), 
                                                       textBgcolor := (pColor $ ss!!(i-1)),
                                                      on command := (varUpdate vstop (const False) 
                                                                     >> varUpdate vis (i:)
                                                                     >> forkIO (prepeatM_ vstop vis (compute1 vis)  vsols)
                                                                     >> repaint p)]
                                       ) [1..length vsols])
       sistop <- map widget <$> (mapM (\i -> button f [text := ("Stop " ++ show i), 
                                                       textBgcolor := (pColor $ ss!!(i-1)),
                                                       on command := varUpdate vis (filter (/= i)) >> repaint p
                                                      ]
                                       ) [1..length vsols])
       -- brestart <- button f [text := "ReStart ", on command := do {close f ; ss' <- mapM varGet vsols; drawSolutions' (Info filename ss' status)}]
       bstart <- button f [text := "Start ", on command := (varUpdate vstop (const False) >> forkIO (prepeatM_ vstop vis (compute1 vis) vsols) >> repaint p)]
       bstop  <- button f [text := "Stop " , on command := (varUpdate vstop (const True) >> repaint p) ]

       bots <- mapM (makeButtons f p (zip vsols vidxs) vidxs vstats) ss

       set p [font := fontFixed 
             , layout := column 10 [ floatLeft (label fc), row 2 [widget bstart,widget bstop {- , widget brestart -}], row 2 bots, row 2 sibots, row 2 sistop, row 2 cbots, row 2 sbots]
             ]

   where
    -- paintInfo :: (ShowPP b, Eq b, Eq src, HasSpan src) => [Var (Partition src b i e)] -> Var [(src, Status src b)] -> [Var Int] -> DC a -> Rect -> IO ()
    paintInfo f vsols vstats vbsols idxs dc viewArea
      = do sols <- mapM varGet vsols
           idxs <- mapM varGet idxs
           sts  <- varGet vstats 
           mapM_ (paintPartition f vbsols dc viewArea) (zip sols idxs)
           mapM_ (paintStatus dc viewArea) sts

    paintStatus :: (ShowPP b, Eq b, Eq src, HasSpan src) => DC a -> Rect -> (src, Status src b) -> IO ()
    paintStatus dc viewArea (src, SOK _ txt)
      = drawText dc ("SOLVED:" ++ showpp txt) (fst $ pointsSrc $ moveUp 2 src) [color := green]
    paintStatus dc viewArea (src, SClash ss)
      = drawText dc ("UNSOLVED:" ++ unwords (showpp . snd <$> ss)) (fst $ pointsSrc $ moveUp 2 src) [color := red]


    -- paintPartition :: (ShowPP b, Eq b, Eq src, HasSpan src) => DC a -> Rect -> (Partition src b i e, Int) -> IO ()
    paintPartition f _ dc viewArea (p,i) 
      | zlength (pContent p) < i 
      = return ()
    paintPartition f vsols dc viewArea (p,i)
      = do set dc [brushColor := (pColor p), brushKind := BrushSolid]
           mapM_ (drawSol f vsols (show (i-1)) -- ((show $ prevNumber 1 p)) --  ++ " ie " ++ (concatMap showpp (take 2 $ zprev $ pContent p))) 
                          (show (zlength (pContent p) - i)) -- ((show $ nextNumber 1 p)) --  ++ " ie " ++ (concatMap showpp (take 2 $ znext $ pContent p)))
                          (pColor p) (pId p) dc) (zip [1..] (getCurrent i $ pContent p))
--            mapM_ (drawSol ((show $ prevNumber p) ++ " ie " ++ (concatMap showpp (take 10 $ zprev $ pContent p))) 
--                           ((show $ nextNumber p) ++ " ie " ++ (concatMap showpp (take 10 $ znext $ pContent p)))
--                           (pColor p) (pId p) dc) (zcurrent $ pContent p)



type PP = ((Int,Int), (Int,Int))
type PointMap = [((PP,PP), (PP,PP))]
{- 
makePointMap :: (HasSpan src) => [[Sol src a]] -> PointMap
makePointMap xss = uniqueTg [] ps 
  where
    ps = [(hasSpan src, hasSpan tg) | xs<-xss, Sol src tg _ <- xs] 
    uniqueTg acc [] = [] 
    uniqueTg acc ((s,t):sts) 
      | t `srcElem` (snd <$> acc) = 
        let lc' =  (s,downTg (snd <$> acc) t) in 
        ((s,t),lc'):uniqueTg (lc':acc) sts
      | t `lineElemRT` (snd <$> acc) = 
        let lc' =  (s,downTg (snd <$> acc) t) in 
        ((s,t),lc'):uniqueTg (lc':acc) sts
      | otherwise 
      = ((s,t), (s,t)):uniqueTg ((s,t):acc) sts

    downTg acc ((x1,x2),(y1,y2)) = let tmp = ((x1+2,x2),(y1+2,y2)) in 
                                  if tmp `srcElem` acc then downTg acc tmp else tmp 

    srcElem ((x1,x2),(y1,y2)) xs = any (\((x1',x2'),(y1',y2')) -> x1 == x1' && y1 == y1' && x2' == y2') xs
    lineElemRT ((x1,x2),(y1,y2)) xs = any (\((x1',x2'),(y1',y2')) -> x1 == x1' && y1 == y1' && (x2 - x2') < 5) xs



normalize :: (HasSpan src) =>  PointMap -> [Sol src a] -> [Sol ((Int,Int), (Int,Int)) a]
normalize mp  = map go 
  where
    go (Sol src tg x) = let (src',tg') = tx (src, tg) in Sol src' tg' x 
    tx (x,y) = fromMaybe (hasSpan x, hasSpan y) (L.lookup (hasSpan x, hasSpan y) mp) 

-}

nextSols :: (ShowPP b, Eq b, Eq src, HasSpan src) => Int -> [Var Int] -> [(Var (Partition src b i e), Var Int)] -> Var [(src, Status src b)] -> Panel () -> IO ()
nextSols i _ sols status p 
      = do let (ss, idx) = sols!!(i-1)
           ssc <- varGet ss 
           varUpdate idx (\i -> (if i < zlength (pContent ssc) then (i+1) else i ))
           -- varUpdate ss (\p -> p{pContent = moveNext (pContent p)})
           ps <- mapM varGet (fst <$> sols) 
           is <- mapM varGet (snd <$> sols) 
           varUpdate status (const $ makeStatus is ps)
           repaint p 

prevSols :: (ShowPP b, Eq b, Eq src) => Int -> [Var Int] -> [(Var (Partition src b i e), Var Int)] -> Var [(src, Status src b)] -> Panel () -> IO ()
prevSols i vis sols status p 
      = do let (_, idx) = sols!!(i-1)
           -- ssc <- varGet ss 
           varUpdate idx (\i -> (if 1 < i then (i-1) else i))
           -- varUpdate ss (\p -> p{pContent = movePrev  (pContent p)})
           ps <- mapM varGet (fst <$> sols) 
           is <- mapM varGet vis 
           varUpdate status (const $ makeStatus is ps)
           repaint p 





{- 
txtline x = 25 + x * 13
txtcolumn x = 2 + 6 * x 
-}

drawSrc :: HasSpan src => Color -> DC a -> src -> IO () 
drawSrc c dc src 
  = line dc s t [brushKind := BrushSolid, penColor := c] 
  where
    (s,t) = pointsSrc src 

drawConnectSrc :: HasSpan src => Color -> DC a -> src -> src -> IO () 
drawConnectSrc c dc src tg
  = line dc p1 p2 [brushKind := BrushHatch HatchVertical, penColor := c]
  where
    (p1, p2) = connect src tg 

connect :: HasSpan src => src -> src -> (Point, Point) 
connect src1 src2 = (p1, p2)
  where
    (Point y1 x1, Point z1 _) = pointsSrc src1 
    (Point y2 x2, Point z2 _) = pointsSrc src2
    p1 = (Point (y1 + (z1 - y1) `div` 2) x1)
    p2 = (Point (y2 + (z2 - y2) `div` 2) x2)


-- drawSol :: (HasSpan src, ShowPP a) => String -> String -> Color -> Int -> DC b -> (Int, Sol src a) -> IO ()
drawSol f ssns pn nn c i dc (j, Sol src tg txt) = do 
  drawSrc c dc src 
  drawSrc c dc tg 
  drawConnectSrc c dc src tg 
 --    >> drawText dc () ((\(Point x y) -> Point x (y+8)) $ fst $ pointsSrc tg) [color := c]
  drawText dc msg (fst $ pointsSrc tg) [color := c]
  sns <- varGet ssns
  if (src, 1) `elem` sns 
         then return ()
         else (if (src, 0) `elem` sns 
          then do { varUpdate ssns ((src,1):)  
                  ; _ <- button f [text := "Match", position := fst $ pointsSrc src, on command := putStrLn ("Button for " ++ showpp src)] 
                  ; return ()
                 }
           else (varUpdate ssns ((src,0):)) >> return ())
  where 
    msg = show i ++ " [ " ++ pn ++ " | " ++  nn ++ " ]" ++ showpp txt

makeBox f e = (e,) <$> checkBox f [text := showpp e , on command := putStrLn ("CHECKED:" ++ showpp e)]


showWindow vp vidx  =  do 
       par <- varGet vp 
       idx <- varGet vidx
       f <- frame    [text := "Show Solution for " ++ show (pId par) ]

       -- create a panel to draw in.
       p <- panel f []

       let sol = getCurrent idx (pContent par)
-- label
       quit <- button f [text := "Quit", on command := close f]
       set f [layout := column 3 [widget quit, widget $ label (showpp sol) ]]



showButtons f p (vp, vidx)  =  do 
  par <- varGet vp 
  cb <- button f [text := "Solution " ++ show (pId par), textBgcolor := (pColor par), on command := showWindow vp vidx]
  return $ widget cb 


choiseWindow vp par =  do 
       f <- frame    [text := "Choose Elements for " ++ show (pId par) ]

       -- create a panel to draw in.
       p <- panel f []

       t <- timer f [interval := 20]

       let elems = pElems $ wl $ pWorkList par
       boxes <- mapM (\(i,es) -> (i,) <$> (mapM (makeBox f) es)) $ filter (not . null . snd ) elems 

       let cboxs = map (\(i,es) -> (boxed ("Pick Elements for\n" ++ showpp i) $ row 2 ((widget . snd) <$> es))) boxes


       quit <- button f [text := "Done", on command := (updateElems boxes >> close f)]
       set f [layout := column 3 [widget quit, column 3 cboxs]]
  where
    updateElems boxes = do 
      elems <- mapM (\(i,es) -> ((i,) . map fst) <$> filterM (checkBoxGetValue . snd) es ) boxes
      putStrLn ("ELEMS = " ++ unlines ((\(i,es) -> showpp i ++ "\n\n" ++ showpp es) <$> elems))
      varUpdate vp (\p -> updateElemsP p (initPick elems))
    updateElemsP p es = p{pState = PInit, pContent = ZEmpty, pWorkList = updateElemsWL (pWorkList p) es}
    updateElemsWL w es = w{wl = es}


statusButton f p src = do 
   cb <- button f [text := "Match ??" ++ showpp src, on command := putStrLn ("Trying to match for " ++ showpp (src))]
   return $ widget cb  

chooseButtons f _ (p, vp) = do 
  cb <- button f [text := "Elements " ++ show (pId p), textBgcolor := (pColor p), on command := choiseWindow vp p]
  return $ widget cb 

makeButtons f p vballs vis vstats par = do
  next <- button f [text := "Next " ++ show i, textBgcolor := c, on command := nextSols i vis vballs vstats p]
  prev <- button f [text := "Prev " ++ show i, textBgcolor := c, on command := prevSols i vis vballs vstats p]
  return $ column 2 [widget next,widget prev]
  where
    i = pId    par 
    c = pColor par 

