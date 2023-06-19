{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Arrows
  ( svgArrows
  , curvyArrowLeft
  , curvyArrowRight
  , bigArrowLeft
  , bigArrowRight
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils


{- |
A list with all the icons of this module, 
together with appropriate names.
This module contains icons suitable for the 
tool bars of a text editor (bold, italic, etc.)

>svgArrows :: [ (String , S.Svg) ]
>svgArrows =
>  [ (,) "curvyArrowLeft" curvyArrowLeft
>  , (,) "curvyArrowRight" curvyArrowRight
>  , (,) "bigArrowLeft"    bigArrowLeft
>  , (,) "bigArrowRight"   bigArrowRight
>  ]
-}
svgArrows :: [ (String , S.Svg) ]
svgArrows =
  [ (,) "curvyArrowLeft"  curvyArrowLeft
  , (,) "curvyArrowRight" curvyArrowRight
  , (,) "bigArrowLeft"    bigArrowLeft
  , (,) "bigArrowRight"   bigArrowRight
  ]


--------------------------------------------------------------------------------


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/curvyArrowLeft_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/curvyArrowLeft_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/curvyArrowLeft_strk.svg)
-}
curvyArrowLeft :: S.Svg
curvyArrowLeft =
    S.path
      ! A.class_ "HaskellSvgIcons__curvyArrowLeft"
      ! d dirs
      ! strokeLinejoin "round"
  where
    r1 = 0.5
    r2 = 0.66
    rm = (r2 - r1)
    k1 = 0.24
    k2 = k1 + rm/2
    dirs = mkPath $ do
      m   (-r1) 0
      aa  (rm/2) (rm/2)  0 False False (-r2)   0
      aa   r2     r2     0 True  False   0   (-r2)
      lr    k1  (-k1)
      lr  (-rm)   0
      lr  (-k2)   k2
      lr    k2    k2
      lr    rm    0
      lr  (-k1) (-k1)
      aa   r1     r1     0 True  True  (-r1)   0
      S.z
    


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/curvyArrowRight_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/curvyArrowRight_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/curvyArrowRight_strk.svg)
-}
curvyArrowRight :: S.Svg
curvyArrowRight =
    S.path
      ! A.class_ "HaskellSvgIcons__curvyArrowRight"
      ! d dirs
      ! strokeLinejoin "round"
  where
    r1 = 0.5
    r2 = 0.66
    rm = (r2 - r1)
    k1 = 0.24
    k2 = k1 + rm/2
    dirs = mkPath $ do
      m   ( r1) 0
      aa  (rm/2) (rm/2)  0 False True  ( r2)   0
      aa   r2     r2     0 True  True    0   (-r2)
      lr  (-k1)  (-k1)
      lr  ( rm)   0
      lr  ( k2)   k2
      lr  (-k2)   k2
      lr  (-rm)   0
      lr  ( k1) (-k1)
      aa   r1     r1     0 True  False ( r1)   0
      S.z
    


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/bigArrowLeft_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/bigArrowLeft_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/bigArrowLeft_strk.svg)
-}
bigArrowLeft :: S.Svg
bigArrowLeft =
    S.path
      ! A.class_ "HaskellSvgIcons__bigArrowLeft"
      ! d dirs
      ! strokeLinejoin "round"
  where
    k0 = 0.2
    y0 = 0.6
    x0 = 0.9
    x1 = 0.5
    x2 = 0.4
    x3 = x1 - 2*k0
    dirs = mkPath $ do
      m  ( x0) (-k0)
      l  (-x2) (-k0)
      l  (-x3) (-y0)
      l  (-x1) (-y0)
      l  (-x0)   0
      l  (-x1) ( y0)
      l  (-x3) ( y0)
      l  (-x2) ( k0)
      l  ( x0) ( k0)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/bigArrowRight_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/bigArrowRight_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/arrows/bigArrowRight_strk.svg)
-}
bigArrowRight :: S.Svg
bigArrowRight =
    S.path
      ! A.class_ "HaskellSvgIcons__bigArrowRight"
      ! d dirs
      ! strokeLinejoin "round"
  where
    k0 = 0.2
    y0 = 0.6
    x0 = 0.9
    x1 = 0.5
    x2 = 0.4
    x3 = x1 - 2*k0
    dirs = mkPath $ do
      m  (-x0) (-k0)
      l  ( x2) (-k0)
      l  ( x3) (-y0)
      l  ( x1) (-y0)
      l    x0    0
      l  ( x1) ( y0)
      l  ( x3) ( y0)
      l  ( x2) ( k0)
      l  (-x0) ( k0)
      S.z