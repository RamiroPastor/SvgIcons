{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Outdoors
  ( svgOutdoors
  , flag
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils


{- |
A list with all the icons of this module, 
together with appropriate names.

>svgOutdoors :: [ (String , S.Svg) ]
>svgOutdoors =
>  [ (,) "flag" flag
>  ]
-}
svgOutdoors :: [ (String , S.Svg) ]
svgOutdoors =
  [ (,) "flag" flag
  ]


--------------------------------------------------------------------------------


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/outdoors/flag_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/outdoors/flag_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/outdoors/flag_strk.svg)
-}
flag :: Svg
flag =
    S.g 
      ! A.class_ "HaskellSvgIcons__flag"
      $ do
        S.path 
          ! A.d staff
        S.path
          ! A.d field
  where
    x1  = -0.85
    x2  = -0.7
    y1  = -0.9
    y2  = -y1
    x3  =  0.75
    x13 =  x2 +     (x3 - x2) / 3
    x23 =  x2 + 2 * (x3 - x2) / 3
    y3  = -0.7
    y4  =  0.2
    ky  =  0.3
    staff = mkPath $ do
      m   x1   y1
      l   x1   y2
      l   x2   y2
      l   x2   y1
      S.z
    field = mkPath $ do
      m   x2    y3
      c   x13  (y3 - ky)  x23  (y3 + ky)  x3   y3
      l   x3    y4
      c   x23  (y4 + ky)  x13  (y4 - ky)  x2   y4
      S.z
