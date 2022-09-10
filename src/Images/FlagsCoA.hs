{-# LANGUAGE     OverloadedStrings       #-}



module Images.FlagsCoA where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Core.Geometry
import Core.Utils


--------------------------------------------------------------------------------


-- Flag of Slovakia coat of arms
-- WORK IN PROGRESS
skCoA :: Svg
skCoA =
    S.path
      ! A.fill "#EE1C25"
      ! A.stroke "#FFFFFF"
      ! (A.strokeWidth .: 2*s)
      ! A.d coatDirs
  where
    s = 0.09
    k1 = 3
    y1 = 6
    k2 = 2.5
    y2 = 8.3
    cm = 5.77
    cw = 2.77
    coatDirs = mkPath $ do
      m   (3 - s )  (3 - s)
      c   (cm - k1)  y1  (cm - k2)  y2   cm           (9 + s)
      c   (cm + k2)  y2  (cm + k1)  y1  (cm + cw + s) (3 - s)
      S.z