module Graphics.UI.Select.DrawSolutions where
import Graphics.UI.WX

import Graphics.UI.WXCore.WxcClassesAL

import Graphics.UI.Select.Types




drawSolutions :: (ShowPP v, Eq v, Eq src, HasSpan src) =>  Info src v -> IO ()
drawSolutions (Info filename ss status)
  = start $ do -- a list of balls, where each ball is represented
       -- by a list of all future positions.
       vsols <- varCreate ss
       vstats <- varCreate status 



       -- create a non-user-resizable top-level (orphan) frame.
       f <- frame    [text := "Interactive Inference"]

       -- create a panel to draw in.
       fc <- readFile filename
       p <- panel f [on paint := paintInfo vsols vstats]

       t <- timer f [interval := 20] 

       bots <- mapM (makeButtons f p vsols vstats) ss

       set p [font := fontFixed 
             , layout := column 80 [ floatLeft (label fc), row 10 bots]
             ]

   where
    paintInfo :: (ShowPP b, Eq b, Eq src, HasSpan src) =>  Var ([Partition src b]) -> Var [(src, Status src b)] -> DC a -> Rect -> IO ()
    paintInfo vsols vstats dc viewArea
      = do sols <- varGet vsols
           sts  <- varGet vstats 
           mapM_ (paintPartition dc viewArea) sols
           mapM_ (paintStatus dc viewArea) sts

    paintStatus :: (ShowPP b, Eq b, Eq src, HasSpan src) => DC a -> Rect -> (src, Status src b) -> IO ()
    paintStatus dc viewArea (src, SOK _ txt)
      = drawText dc ("SOLVED:" ++ showpp txt) (fst $ pointsSrc $ moveUp 2 src) [color := green]
    paintStatus dc viewArea (src, SClash ss)
      = drawText dc ("UNSOLVED:" ++ unwords (showpp . snd <$> ss)) (fst $ pointsSrc $ moveUp 2 src) [color := red]


    paintPartition :: (ShowPP b, Eq b, Eq src, HasSpan src) => DC a -> Rect -> Partition src b -> IO ()
    paintPartition dc viewArea (Partition i c s)
      = do set dc [brushColor := c, brushKind := BrushSolid]
           mapM_ (drawSol c i dc) (current s)


nextSols :: (ShowPP b, Eq b, Eq src, HasSpan src) => Int -> Var [Partition src b] -> Var [(src, Status src b)] -> Panel () -> IO ()
nextSols i sols status p 
      = do varUpdate sols (movePNext i)
           ps <- varGet sols 
           varUpdate status (const $ makeStatus ps)
           repaint p 

prevSols :: (ShowPP b, Eq b, Eq src) => Int -> Var [Partition src b] -> Var [(src, Status src b)] -> Panel () -> IO ()
prevSols i sols status p 
      = do varUpdate sols (movePPrev i)
           ps <- varGet sols 
           varUpdate status (const $ makeStatus ps)
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


drawSol :: (HasSpan src, ShowPP a) => Color -> Int -> DC b -> Sol src a -> IO ()
drawSol c i dc (Sol src tg txt)
  =  drawSrc c dc src >> drawSrc c dc tg >> drawConnectSrc c dc src tg 
    >> drawText dc (show i ++ ": " ++ showpp txt) (fst $ pointsSrc tg) [color := c]



makeButtons f p vballs vstats (Partition i c _) = do
  next <- button f [text := "Next " ++ show i, textBgcolor := c, on command := nextSols i vballs vstats p]
  prev <- button f [text := "Prev " ++ show i, textBgcolor := c, on command := prevSols i vballs vstats p]
  return $ column 2 [widget next,widget prev]

