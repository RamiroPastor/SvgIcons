{-# LANGUAGE     OverloadedStrings       #-}


{- |
Module for logos.
-}
module SvgIcons.Images.Logos 
  ( logos
  , haskell
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Geometry
import SvgIcons.Core.Utils


{- |
A list with all the logos from this module,
together with appropriate names.

>logos :: [ (String , S.Svg) ]
>logos =
>  [ (,) "haskell" haskell
>  ]
-}
logos :: [ (String , S.Svg) ]
logos =
  [ (,) "haskell" haskell
  ]


{- |
Haskell logo

viewbox is "0 0 17 12"

![Haskell logo](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/logos/haskell.svg)
-}
haskell :: Svg
haskell =
    svg
      ! A.viewbox "0 0 17 12"
      ! A.height "120px"
      ! A.width  "170px"
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
    x1  = 0
    x2  = 3
    x3  = 4
    x4  = 7
    x5  = 8
    x6  = 9.5
    x7  = 10.33
    x8  = 11.66
    x9  = 12
    x10 = 12.33
    x11 = 13.66
    x12 = 15
    x13 = 17
    y1  = 0
    y2  = 3.5
    y3  = 5.5
    y4  = 6
    y5  = 6.5
    y6  = 8.25
    y7  = 8.5
    y8  = 12
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