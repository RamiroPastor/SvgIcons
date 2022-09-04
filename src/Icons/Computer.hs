{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Computer where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgComputer :: [ (String , S.Svg) ]
svgComputer =
  [ (,) "plus"      plus
  , (,) "cancel"    cancel
  , (,) "accept"    accept
  , (,) "warning"   warning
  , (,) "minimize"  minimize
  , (,) "maximize"  maximize
  , (,) "menuDots"  menuDots
  , (,) "menuLines" menuLines
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
      

minimize :: Svg
minimize =
  S.path
    ! A.d dirs
  where
    w = 0.1
    k = 0.7
    dirs = mkPath $ do
      m   (-k)  (-w)
      aa  ( w)  ( w)  0  True  False  (-k) ( w)
      l   ( k)  ( w)
      aa  ( w)  ( w)  0  True  False  ( k) (-w)
      S.z



maximize :: Svg
maximize =
  S.g $ do
    S.path
      ! A.d dirs1
      ! A.transform (translate (k - 0.15) (0.15 - k))
      ! A.stroke "none"
      ! A.opacity "0.5"
    S.path
      ! A.d dirs2
      ! A.transform (translate (  - 0.15)  0.15)
      ! A.fill "none"
    S.path
      ! A.d dirs1
      ! A.transform (translate (-0.1 ) 0.1)
  where
    w = 1.4
    k = 0.25
    dirs1 = mkPath $ do
      m   (-0.5 * w)  (-0.5 * w)
      l   ( 0.5 * w)  (-0.5 * w)
      l   ( 0.5 * w)  ( 0.5 * w)
      l   (-0.5 * w)  ( 0.5 * w)
      S.z
    dirs2 = mkPath $ do
      m   (-0.5*w + k)  (-0.5*w - k)
      l   ( 0.5*w + k)  (-0.5*w - k)
      l   ( 0.5*w + k)  ( 0.5*w - k)


menuDots :: Svg
menuDots =
    S.g $ do
        dot (-0.7)
        dot ( 0  )
        dot ( 0.7)
  where
    dot y =
      circle 
        ! (A.cy .: y)
        ! A.cx "0"
        ! A.r  "0.2"

  
menuLines :: Svg
menuLines =
    S.g $ do
      S.path ! A.d (line (-0.5))
      S.path ! A.d (line ( 0  ))
      S.path ! A.d (line ( 0.5))
  where
    kx = 0.7
    r  = 0.12
    line y = mkPath $ do
      m  (-kx)  (y - r)
      aa   r   r   0   True  False (-kx) (y + r)
      l  ( kx)  (y + r)
      aa   r   r   0   True  False ( kx) (y - r)
      S.z