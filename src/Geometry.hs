{-# LANGUAGE     OverloadedStrings       #-}



module Geometry where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



--------------------------------------------------------------------------------


evenOddSplit :: [a] -> ([a], [a])
evenOddSplit [] = ([], [])
evenOddSplit (x:xs) = (x:o, e)
  where (e,o) = evenOddSplit xs



starPolygonFirstSpecies :: 
  Int -> Float -> (Float, Float) -> Svg
starPolygonFirstSpecies n r (c1,c2) =
    S.path
      ! d directions
  where
    α  = 2 * pi / (fromIntegral n)
    vertice k' = 
      let k = fromIntegral k'
      in 
        (,) (c1 + r * cos (k*α))
            (c2 + r * sin (k*α))
    verticesList = map vertice [0 .. (n-1)]
    directions =
      if even n 
        then 
          mkPath $ do
            m   (fst $ head verticesList)  (snd $ head verticesList)
            mapM_ (uncurry S.l) (fst $ evenOddSplit verticesList)
            S.z
            m   (fst $ verticesList !! 1)  (snd $ verticesList !! 1)
            mapM_ (uncurry S.l) (snd $ evenOddSplit verticesList)
            S.z
        else
          mkPath $ do
            m   (fst $ head verticesList)  (snd $ head verticesList)
            mapM_ (uncurry S.l) (tail $ fst $ evenOddSplit $ verticesList ++ verticesList)
            S.z