{-# LANGUAGE     OverloadedStrings       #-}



module Icons.File where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgFile :: [ (String , S.Svg) ]
svgFile =
  [ (,) "plus"    plus
  , (,) "cancel"  cancel
  , (,) "accept"  accept
  , (,) "warning" warning
  ]


--------------------------------------------------------------------------------


plus :: Svg
plus =
    S.path 
      ! A.d dirs
  where
    k1 = 0.1
    k2 = 0.8
    dirs = mkPath $ do
      m   ( k1) (-k1)
      l   ( k1) (-k2)
      aa    k1    k1   0  True  False (-k1) (-k2)
      l   (-k1) (-k1)
      l   (-k2) (-k1)
      aa    k1    k1   0  True  False (-k2) ( k1)
      l   (-k1) ( k1)
      l   (-k1) ( k2)
      aa    k1    k1   0  True  False ( k1) ( k2)
      l   ( k1) ( k1)
      l   ( k2) ( k1)
      aa    k1    k1   0  True  False ( k2) (-k1)
      S.z


cancel :: Svg
cancel =
  S.g $ 
    plus ! A.transform (rotateAround 45 0 0)


accept :: Svg
accept =
  S.g $ do
    S.path
      ! A.strokeLinejoin "round"
      ! d dirs
      ! A.transform (translate (-0.3) 0.3 <> rotateAround 45 0 0)
    frame (-1) (-1) 2 2
  where
    k1 = 0.1
    k2 = 0.5
    k3 = 1.3
    dirs = mkPath $ do
      m   (-k1) (-k1)
      l   (-k2) (-k1)
      aa    k1    k1   0  True  False (-k2) ( k1)
      l   ( k1) ( k1)
      l   ( k1) (-k3)
      aa    k1    k1   0  True  False (-k1) (-k3)
      S.z


warning :: Svg
warning = 
    S.g $ do
      S.path
        ! d triangleDirs
        ! strokeLinejoin "round"
        ! fillRule "evenodd"
      S.path
        ! d stickPath
      S.circle
        ! (cx .: 0)
        ! (cy .: 0.15)
        ! (r  .: w)
      S.circle
        ! (cx .: 0)
        ! (cy .: 0)
        ! (r  .: 2*ap2)
        ! A.fill "transparent"
  where
    w  = 0.1
    ap1 = 0.36
    ap2 = ap1 + w
    lm1 = (sqrt 3) * ap1
    lm2 = (sqrt 3) * ap2
    y1 = -0.3
    y2 = -0.05
    triangleDirs = mkPath $ do
      m    0    (-2*ap2)
      l  (-lm2) (   ap2)
      l  ( lm2) (   ap2)
      S.z
      m    0    (-2*ap1)
      l  (-lm1) (   ap1)
      l  ( lm1) (   ap1)
      S.z
    stickPath = mkPath $ do
      m   (-w)    y1
      l   (-w/2)  y2
      aa  ( w/2) (w/2)  0  True  False (w/2) y2
      l   ( w)    y1
      aa    w     w     0  True  False (-w)  y1
      S.z
      
