{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Coding
  ( svgCoding
  , haskell
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Geometry
import SvgIcons.Core.Utils


{- |
A list with all the icons from this module,
together with appropriate names.

svgCoding :: [ (String , S.Svg) ]
svgCoding =
  [ (,) "haskell" haskell
  ]
-}
svgCoding :: [ (String , S.Svg) ]
svgCoding =
  [ (,) "haskell" haskell
  ]


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/haskell_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/haskell_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/coding/haskell_strk.svg)
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