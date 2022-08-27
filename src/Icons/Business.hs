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
    leftBuilding
    leftWindows
    leftDoor
    rightBuilding
    rightWindows
  where
    w = 0.02
    x1 = 0.12
    x2 = x1 + 0.1
    x3 = x4 - 0.1
    x4 = 0.6
    x5 = 1 - x1
    y1 = 0.10
    y2 = 0.15
    y3 = (y1 + y4) / 2
    y4 = 0.35
    y5 = y4 + 0.05
    y6 = y7 - 0.05
    y7 = 1 - y1
    doorH = 0.12
    ----------------------------------------
    leftBuilding =
      S.path
        ! d leftBuildingPath
        ! (strokeWidth .: 2*w)
        ! fill "none"
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
        S.z
    rightBuilding =
      S.path
        ! d rightBuildingPath
        ! (strokeWidth .: 2*w)
        ! fill "none"
    rightBuildingPath =
      mkPath $ do
        m   x4  y4
        l   x5  y4
        l   x5  y7
        l   x4  y7
    ----------------------------------------
    leftDoor =
      S.path
        ! d (mkPath $   m ((x1+x4)/2) y7 >> l ((x1+x4)/2) (y7 - doorH) )
        ! strokeWidth "0.08"
        ! fill "none"
    ----------------------------------------
    k1 = (x3 - x2) / 3
    leftWindows =
      S.path
        ! d leftWindowsPath
        ! (strokeWidth .: 2*w)
        ! strokeDasharray (S.toValue $ (show $ 3*w) ++ " " ++ (show w))
        ! fill "none"
    leftWindowsPath =
      mkPath $ do
        m   (x2 + 0*k1)  y3
        l   (x2 + 0*k1)  y6
        m   (x2 + 1*k1)  y3
        l   (x2 + 1*k1)  (y7 - doorH)
        m   (x2 + 2*k1)  y3
        l   (x2 + 2*k1)  (y7 - doorH)
        m   (x2 + 3*k1)  y3
        l   (x2 + 3*k1)  y6
    ----------------------------------------
    k2 = ((x5-w) - (x4+w)) / 4
    rightWindows =
      S.path
        ! d rightWindowsPath
        ! (strokeWidth .: 2*w)
        ! (strokeDasharray .: 2*w)
        ! fill "none"
    rightWindowsPath =
      mkPath $ do
        m   (x4 + w + 1*k2)  y5
        l   (x4 + w + 1*k2)  y6
        m   (x4 + w + 2*k2)  y5
        l   (x4 + w + 2*k2)  y6
        m   (x4 + w + 3*k2)  y5
        l   (x4 + w + 3*k2)  y6