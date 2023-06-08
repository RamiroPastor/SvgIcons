{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Coding
  ( svgCoding
  , xmlCode
  , haskell
  , git
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Geometry
import SvgIcons.Core.Utils


{- |
A list with all the icons from this module,
together with appropriate names.

>svgCoding :: [ (String , S.Svg) ]
>svgCoding =
>  [ (,) "xmlCode" xmlCode
>  , (,) "haskell" haskell
>  , (,) "git"     git
>  ]
-}
svgCoding :: [ (String , S.Svg) ]
svgCoding =
  [ (,) "xmlCode" xmlCode
  , (,) "haskell" haskell
  , (,) "git"     git
  ]



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/xmlCode_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/xmlCode_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/xmlCode_strk.svg)
-}
xmlCode :: Svg
xmlCode =
    S.g $ do
      S.defs $ 
        S.path
          ! A.id_ "HaskellSvgIcons-xmlCode-triangle"
          ! A.d triangleDirs
          ! A.strokeLinejoin "round"
      S.use
        ! A.xlinkHref "#HaskellSvgIcons-xmlCode-triangle"
        ! A.transform (rotateAround 315 0 0)
      S.use
        ! A.xlinkHref "#HaskellSvgIcons-xmlCode-triangle"
        ! A.transform (rotateAround 135 0 0)
      S.path
        ! A.d barDirs
        ! A.transform (rotateAround  11 0 0)
  where
    k0 = 0.1
    k1 = 0.5
    k2 = 0.7
    r1 = (k2 - k1) / 2
    k3 = 0.55
    triangleDirs = mkPath $ do
      m   k1   k1
      l   k0   k1
      aa  r1   r1  0  True  False k0  k2
      l   k2   k2
      l   k2   k0
      aa  r1   r1  0  True  False k1  k0
      S.z
    barDirs = mkPath $ do
      m  (-r1)  k3
      aa   r1   r1  0  True  False   r1    k3
      l    r1 (-k3)
      aa   r1   r1  0  True  False (-r1) (-k3)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/haskell_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/haskell_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/haskell_strk.svg)

Note: you can remove fill colors with CSS:

>path {
>  fill: none;
>}
-}
haskell :: Svg
haskell =
    S.g
      $ do
        S.path
          ! A.fill "#453a62"
          ! A.d leftDirs
          ! A.id_ "HaskellSvgIcons-haskellLogo-left"
        S.path
          ! A.fill "#5e5086"
          ! A.d midDirs
          ! A.id_ "HaskellSvgIcons-haskellLogo-mid"
        S.path
          ! A.fill "#8f4e8b"
          ! A.d rightDirs
          ! A.id_ "HaskellSvgIcons-haskellLogo-right"
  where
    ks = 0.112
    tx = -0.5 * ks * 17
    ty = -0.5 * ks * 12
    f x = ks * x + tx
    g y = ks * y + ty
    x1  = f 0
    x2  = f 3
    x3  = f 4
    x4  = f 7
    x5  = f 8
    x6  = f 9.5
    x7  = f 10.33
    x8  = f 11.66
    x9  = f 12
    x10 = f 12.33
    x11 = f 13.66
    x12 = f 15
    x13 = f 17
    y1  = g 0
    y2  = g 3.5
    y3  = g 5.5
    y4  = g 6
    y5  = g 6.5
    y6  = g 8.25
    y7  = g 8.5
    y8  = g 12
    leftDirs = mkPath $ do
      m   x1   y1
      l   x2   y1
      l   x4   y4
      l   x2   y8
      l   x1   y8
      l   x3   y4
      S.z
    midDirs = mkPath $ do
      m   x3   y1
      l   x4   y1
      l   x12  y8
      l   x9   y8
      l   x6   y6
      l   x4   y8
      l   x3   y8
      l   x5   y4
      S.z
    rightDirs = mkPath $ do
      m   x8   y3
      l   x7   y2
      l   x13  y2
      l   x13  y3
      S.z
      m   x11  y7
      l   x10  y5
      l   x13  y5
      l   x13  y7
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/git_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/git_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/git_strk.svg)

Note: if you want to use a stroked version, you can embed this icon in a bigger viewbox such as @"-1.2 -1.2 2.4 2.4"@ or scale down:

> git
>   ! A.transform (S.scale 0.8 0.8)
-}
git :: Svg
git = 
    S.g $
      S.path
        ! A.d dirs
        ! A.fill "#F03C2E"
        ! A.transform (rotateAround 45 0 0)
  where
    r0 =  0.13
    r1 =  0.153
    r2 =  0.149
    m0 =  0.745
    y1 = -0.355
    y2 = -0.23
    x1 = -0.42
    x2 = -0.145
    x3 =  0.17
    k0 = (y2 - y1)
    k1 =  0.38
    dirs = mkPath $ do
      m  (-m0)        y2
      l  (-m0)      ( m0 - r0)
      aa   r0   r0    0  False False (-m0 + r0)   m0
      l  ( m0 - r0)   m0
      aa   r0   r0    0  False False   m0       ( m0 - r0)
      l    m0       (-m0 + r0)
      aa   r0   r0    0  False False ( m0 - r0) (-m0) 
      l  (-m0 + r0) (-m0)
      aa   r0   r0    0  False False (-m0)      (-m0 + r0)
      ---
      l  (-m0)  y1
      l    x1   y1
      aa   r2   r2    0  False True   x2  y1
      l    x3   y1
      aa   r1   r1    0  True  True   x3  y2
      l  ( x2 + 0.02) y2
      lr   k1   k1
      ar   r1   r1    0  True  True  (-0.5 * k0 * sqrt 2) ( 0.5 * k0 * sqrt 2)
      lr (-k1 - 0.01) (-k1 - 0.01)
      aa   r2   r2    0  False True   x1  y2
      S.z


