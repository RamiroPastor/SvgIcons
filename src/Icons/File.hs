{-# LANGUAGE     OverloadedStrings       #-}



module Icons.File where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgFile :: [ (String , S.Svg) ]
svgFile =
  [ (,) "plus"   plus
  , (,) "cancel" cancel
  , (,) "accept" accept
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


exclamationSignal :: Svg
exclamationSignal = 
  svg
    ! A.viewbox "0 0 1 1"
    $ do
      triangle
      stick
      dot
  where
    k1 = 0.1
    k2 = 0.06
    k3 = 0.035
    h1 = (1 - 2*k1)
    y1 = 0.4
    y2 = 0.65
    y3 = 0.78
    triangle =
      S.path
        ! d trianglePath
        ! (strokeWidth .: 1.5 * k1)
        ! strokeLinejoin "round"
    trianglePath = mkPath $ do
      m   0.5    (1 - k1 - h1)
      l   k1     (1-k1)
      l   (1-k1) (1-k1)
      S.z
    stick =
      S.path
        ! d stickPath
        ! fill "white"
        ! (strokeWidth .: 0)
    stickPath = mkPath $ do
      m   (0.5 - k3)  y2
      l   (0.5 + k3)  y2
      l   (0.5 + k2)  y1
      aa  k2  k2  0  True  False  (0.5 - k2)  y1
      S.z
    dot =
      S.circle
        ! (cx .: 0.5)
        ! (cy .: y3)
        ! (r  .: k2)
        ! fill "white"
        ! (strokeWidth .: 0)
