{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Business where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgBusiness :: [ (String , S.Svg) ]
svgBusiness =
  [ (,) "company" company
  ]


--------------------------------------------------------------------------------


company :: S.Svg
company =
  S.g $ do
    S.path
      ! d leftBuildingPath
    S.path
      ! d leftWindowsPath
      ! strokeDasharray "0.12 0.06"
    S.path
      ! d rightBuildingPath
    S.path
      ! d rightWindowsPath
      ! strokeDasharray "0.05"
  where
    x1 = -0.92
    x2 = -0.72
    x3 =  0
    x4 =  0.22
    x5 =  0.92
    y1 = -0.9
    y2 = -0.75
    y3 = (y1 + y4) / 2
    y4 = -0.3
    y5 = -0.2
    y6 =  0.8
    y7 =  0.9
    k1 = (x3 - x2) / 3
    k2 = (x5 - x4) / 4
    doorH = 0.24
    ----------------------------------------
    leftBuildingPath =
      mkPath $ do
        m   x1  y7
        l   x1  y2
        l   x2  y2
        l   x2  y1
        l   x3  y1
        l   x3  y2
        l   x4  y2
        l   x4  y7
        doorPath
        S.z
    rightBuildingPath =
      mkPath $ do
        m   x4  y4
        l   x5  y4
        l   x5  y7
        l   x4  y7
        S.z
    doorPath = 
      do
        l (x2 + 2*k1)  y7
        l (x2 + 2*k1) (y7 - doorH)
        l (x2 +   k1) (y7 - doorH)
        l (x2 +   k1)  y7
    leftWindowsPath =
      mkPath $ do
        m   (x2 + 0*k1)  y3
        l   (x2 + 0*k1)  y6
        m   (x2 + 1*k1)  y3
        l   (x2 + 1*k1)  (y7 - 1.5 * doorH)
        m   (x2 + 2*k1)  y3
        l   (x2 + 2*k1)  (y7 - 1.5 * doorH)
        m   (x2 + 3*k1)  y3
        l   (x2 + 3*k1)  y6
    rightWindowsPath =
      mkPath $ do
        m   (x4 + 1*k2)  y5
        l   (x4 + 1*k2)  y6
        m   (x4 + 2*k2)  y5
        l   (x4 + 2*k2)  y6
        m   (x4 + 3*k2)  y5
        l   (x4 + 3*k2)  y6