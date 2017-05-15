{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main where
  
import Graphics.UI.WX
import Graphics.UI.WXCore.WxcClassesAL

import Graphics.UI.Select.Types
import Graphics.UI.Select.DrawSolutions



ss1 = [[[Sol (SrcSpan (6,22) (6,23)) (SrcSpan (8, 25) (8,31)) "v<0",
                          Sol (SrcSpan (6,22) (6,23)) (SrcSpan (8, 37) (8,43)) "v<0" ], 
                          [Sol (SrcSpan (6,22) (6,23)) (SrcSpan (8, 25) (8,31)) "v=0",
                          Sol (SrcSpan (6,22) (6,23)) (SrcSpan (8, 37) (8,43)) "v=0" ]]]
ss2 = [ [[Sol (SrcSpan (6,22) (6,23)) (SrcSpan (8, 25) (8,31)) "v<0"], [Sol (SrcSpan (6,22) (6,23)) (SrcSpan (8, 25) (8,31)) "v=0"]]
      , [[Sol (SrcSpan (6,22) (6,23)) (SrcSpan (8, 37) (8,43)) "v<0"], [Sol (SrcSpan (6,22) (6,23)) (SrcSpan (8, 37) (8,43)) "v=0" ]]
      ]

--the main function
main :: IO () 
main = do 
  let fname = "/Users/niki/liquidtypes/liquidhaskell/tests/gradual/pos/Gradual.hs"
  return () -- drawSolutions $ error "?" -- (makePInfo fname ss2)
                         
instance ShowPP String where
  showpp x = x  
