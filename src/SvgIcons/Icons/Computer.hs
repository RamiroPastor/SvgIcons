{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Computer 
  ( svgComputer
  , ring
  , accept
  , cancel
  , plus
  , maximize
  , minimize
  , menuDots
  , menuLines
  , powerButton 
  , warning
  , diskette
  , save
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils



{- |
A list with all the icons of this module, 
together with appropriate names.

>svgComputer :: [ (String , S.Svg) ]
>svgComputer =
>  [ (,) "ring"        ring
>  , (,) "accept"      accept
>  , (,) "cancel"      cancel
>  , (,) "plus"        plus
>  , (,) "maximize"    maximize
>  , (,) "minimize"    minimize
>  , (,) "menuDots"    menuDots
>  , (,) "menuLines"   menuLines
>  , (,) "powerButton" powerButton 
>  , (,) "warning"     warning
>  , (,) "diskette"    diskette
>  , (,) "save"        save
>  ]
-}
svgComputer :: [ (String , S.Svg) ]
svgComputer =
  [ (,) "ring"        ring
  , (,) "accept"      accept
  , (,) "cancel"      cancel
  , (,) "plus"        plus
  , (,) "maximize"    maximize
  , (,) "minimize"    minimize
  , (,) "menuDots"    menuDots
  , (,) "menuLines"   menuLines
  , (,) "powerButton" powerButton 
  , (,) "warning"     warning
  , (,) "diskette"    diskette
  , (,) "save"        save
  ]


--------------------------------------------------------------------------------


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/ring_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/ring_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/ring_strk.svg)
-}
ring :: Svg
ring =
  S.g $ do
    S.path
      ! A.d dirs
  where
    r1 = 0.75
    r2 = 0.95
    dirs = mkPath $ do
      m   r1  0
      aa  r1  r1  0  True  False  r1  0.0001
      S.z
      m   r2  0
      aa  r2  r2  0  True  True   r2  (-0.0001)
      S.z





{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/accept_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/accept_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/accept_strk.svg)
-}
accept :: Svg
accept =
  S.g $ do
    S.path
      ! A.strokeLinejoin "round"
      ! d dirs
      ! A.transform (translate (-0.3) 0.3 <> rotateAround 45 0 0)
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/plus_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/plus_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/plus_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/cancel_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/cancel_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/cancel_strk.svg)
-}
cancel :: Svg
cancel =
  S.g $ 
    plus ! A.transform (rotateAround 45 0 0)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/warning_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/warning_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/warning_strk.svg)
-}
warning :: Svg
warning = 
  S.g $ 
    S.g 
      ! A.transform (translate 0 0.2)
      $ do
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
    ap1 = 0.42
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
      


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/minimize_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/minimize_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/minimize_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/maximize_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/maximize_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/maximize_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/menuDots_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/menuDots_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/menuDots_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/menuLines_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/menuLines_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/menuLines_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/powerButton_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/powerButton_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/powerButton_strk.svg)
-}
powerButton :: S.Svg
powerButton =
  S.g $ do
    S.path
      ! d innerCircle
      ! A.transform (translate 0 0.1)
    S.path
      ! d littleStickPath
      ! A.transform (translate 0 (-0.42))
  where
    w  = 0.08
    r1 = 0.7
    r2 = r1 + 2*w
    α  = pi / 8
    y1 = 0.4
    innerCircle =
      mkPath $ do
        m   (r1 * sin α)  (-r1 * cos α)
        aa   w   w   0   True  True  ( r2 * sin α) (-r2 * cos α)
        aa   r2  r2  0   True  True  (-r2 * sin α) (-r2 * cos α)
        aa   w   w   0   True  True  (-r1 * sin α) (-r1 * cos α)
        aa   r1  r1  0   True  False ( r1 * sin α) (-r1 * cos α)
        S.z
    littleStickPath =
      mkPath $ do
        m     w  (-y1)
        aa    w    w   0   True  False (-w) (-y1)
        l   (-w) ( y1)
        aa    w    w   0   True  False ( w) ( y1)
        S.z
      
    
  
{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/diskette_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/diskette_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/diskette_strk.svg)
-}
diskette :: Svg
diskette =
    S.path
      ! A.d shell
      ! A.fillRule "evenodd"
  where
    k0 = 0.9
    r0 = 0.06
    r1 = 3*r0
    (px,py) = (0    ,-0.55)
    (pw,ph) = (0.5  , 0.29)
    (qx,qy) = (0    , 0.35)
    (qw,qh) = (0.65 , 0.45)
    (tx,ty) = (0.2  ,-0.54)
    (tw,th) = (0.1  , 0.23)
    rq = 0.05
    shell = mkPath $ do
      m       0     (-k0     )
      l  (-k0 + r0) (-k0     )
      aa   r0   r0   0   False  False  (-k0     ) (-k0 + r0)
      l  (-k0     ) ( k0 - r0)
      aa   r0   r0   0   False  False  (-k0 + r0) ( k0     )
      l  ( k0 - r0) ( k0     )
      aa   r0   r0   0   False  False  ( k0     ) ( k0 - r0)
      l  ( k0     ) (-k0 + r1)
      l  ( k0 - r1) (-k0     )
      S.z
      rectangleWithRoundCorners rq   (qw,qh) (qx,qy)
      rectangleWithRoundCorners rq   (pw,ph) (px,py)
      rectangleWithRoundCorners 0.01 (tw,th) (tx,ty)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/save_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/save_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/computer/save_strk.svg)
-}
save :: Svg
save =
  S.g $ do
    diskette
    accept 
      ! A.transform (translate 0 0.4 <> S.scale 0.5 0.5)