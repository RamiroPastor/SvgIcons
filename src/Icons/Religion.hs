{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Religion 
  ( svgReligion
  , xp
  , taijitu
  , crossLatin
  , crossOrthodox
  , crescentAndStar
  , starOfDavid
  , iChingHexagram
  ) where

import           Data.String
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Core.Geometry
import Core.Utils



{- |
A list with all the icons of this module, 
together with appropriate names.

>svgReligion :: [ (String , S.Svg) ]
>svgReligion =
>  [ (,) "xp"               xp
>  , (,) "taijitu"         (taijitu "black" "white")
>  , (,) "crossLatin"       crossLatin
>  , (,) "crossOrthodox"    crossOrthodox
>  , (,) "crescentAndStar"  crescentAndStar
>  , (,) "starOfDavid"      starOfDavid
>  , (,) "exampleHexagram" (iChingHexagram (8,8,7,8,7,7))
>  ]
-}
svgReligion :: [ (String , S.Svg) ]
svgReligion =
  [ (,) "xp"               xp
  , (,) "taijitu"         (taijitu "black" "white")
  , (,) "crossLatin"       crossLatin
  , (,) "crossOrthodox"    crossOrthodox
  , (,) "crescentAndStar"  crescentAndStar
  , (,) "starOfDavid"      starOfDavid
  , (,) "exampleHexagram" (iChingHexagram (8,8,7,8,7,7))
  ]


--------------------------------------------------------------------------------



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/xp_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/xp_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/xp_strk.svg)
-}
xp :: Svg
xp = 
  S.g $ do
    S.path
      ! A.d rho
      ! A.fillRule "evenodd"
    S.path 
      ! A.d chi
      ! A.transform (rotateAround 45 0 0)
    S.path
      ! A.d alpha
      ! A.fill "none"
      ! A.strokeLinecap  "round"
      ! A.strokeLinejoin "round"
    S.path
      ! A.d omega
      ! A.fill "none"
      ! A.strokeLinecap  "round"
      ! A.strokeLinejoin "round"
  where
    w = 0.07
    k1 = 0.5
    k2 = 0.96
    k3 = 0.17
    r1 = 0.2
    a  = 0.3
    chi = mkPath $ do
      m   (   -w)  (   -w)
      l   (   -w)  (-k1+w)
      q   (   -w)  (-k1  )  ( -2*w)  (-k1  )
      l   (  2*w)  (-k1  )
      q   (    w)  (-k1  )  (    w)  (-k1+w)
      l   (    w)  (   -w)
      l   ( k1-w)  (   -w)
      q   ( k1  )  (   -w)  ( k1  )  ( -2*w)
      l   ( k1  )  (  2*w)
      q   ( k1  )  (    w)  ( k1-w)  (    w)
      l   (    w)  (    w)
      l   (    w)  ( k1-w)
      q   (    w)  ( k1  )  (  2*w)  ( k1  )
      l   ( -2*w)  ( k1  )
      q   (   -w)  ( k1  )  (   -w)  ( k1-w)
      l    (  -w)  (    w)
      l   (-k1+w)  (    w)
      q   (-k1  )  (    w)  (-k1  )  (  2*w)
      l   (-k1  )  ( -2*w)
      q   (-k1  )  (   -w)  (-k1+w)  (   -w)
      S.z
    rho = mkPath $ do
      m   (    w)  (-k2  )
      l   ( -2*w)  (-k2  )
      q   (   -w)  (-k2  )  (   -w)  (-k2+w)
      l   (   -w)  ( k2-w)
      q   (   -w)  ( k2  )  ( -2*w)  ( k2)
      l   (  2*w)  ( k2  )
      q   (    w)  ( k2  )  (    w)  ( k2-w)
      l   (    w)  (-k2+2*r1)
      l   ( k3+w)  (-k2+2*r1)
      aa   r1   r1   0   True  False ( k3+w)  (-k2)
      S.z
      m   (    w)  (-k2+2*r1 - w)
      c   (2*k3+w)  (-k2+2*r1)  (2*k3+w)  (-k2)  ( w) (-k2 + w)
      S.z
    alpha = mkPath $ do
      m  (-0.7-a/2)  ( a/2)
      l  (-0.7    )  (-a/2)
      l  (-0.7+a/2)  ( a/2)
      m  (-0.7-a/4)  ( 0  )
      l  (-0.7    )  ( a/4)
      l  (-0.7+a/4)  ( 0  )
    omega = mkPath $ do
      m  ( 0.7-a/2)  ( a/2)
      l  ( 0.7-a/5)  ( a/2)
      aa (a/2)  (a/2)  0  True  True  (0.7+a/5) (a/2)
      l  ( 0.7+a/2)  ( a/2)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/taijitu_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/taijitu_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/taijitu_strk.svg)

You must provide both yin and yang colors
-}
taijitu :: String -> String -> Svg
taijitu yinColor yangColor =
    S.g $ do
      outerCircle
      yin
      yangDot
      yinDot
  where
    r1  = 0.92
    r1m = 0.5 * r1
    r2  = r1 / 6
    outerCircle =
      S.circle
        ! (A.cx .: 0)
        ! (A.cy .: 0)
        ! (A.r  .: r1)
        ! A.fill (S.toValue yangColor)
    yin =
      S.path
        ! A.fill (S.toValue yinColor)
        ! A.d yinDirs
    yinDirs = mkPath $ do
      m   ( -r1)  0
      aa  r1m  r1m  0  True  False   0    0
      aa  r1m  r1m  0  True  True  ( r1)  0
      aa  r1   r1   0  True  True  (-r1)  0
      S.z
    yangDot =
      S.circle
        ! (A.cx .: 0 + r1m)
        ! (A.cy .: 0 )
        ! (A.r  .: r2)
        ! A.stroke "none"
        ! A.fill (S.toValue yangColor)
    yinDot =
      S.circle
        ! (A.cx .: 0 - r1m)
        ! (A.cy .: 0 )
        ! (A.r  .: r2)
        ! A.stroke "none"
        ! A.fill (S.toValue yinColor)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crossLatin_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crossLatin_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crossLatin_strk.svg)
-}
crossLatin :: Svg
crossLatin =
    S.path
      ! A.d dirs
  where
    w  = 0.1
    k1 = 0.9
    k2 = k1 * 3/4
    km = -k1 + k1 * 2/3
    dirs = mkPath $ do
      m  (-w )  (-k1    )
      l  (-w )  ( km - w)
      l  (-k2)  ( km - w)
      l  (-k2)  ( km + w)
      l  (-w )  ( km + w)
      l  (-w )  ( k1    )
      l  ( w )  ( k1    )
      l  ( w )  ( km + w)
      l  ( k2)  ( km + w)
      l  ( k2)  ( km - w)
      l  ( w )  ( km - w)
      l  ( w )  (-k1    )
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crossOrthodox_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crossOrthodox_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crossOrthodox_strk.svg)
-}
crossOrthodox :: Svg
crossOrthodox =
    S.path
      ! A.d dirs
  where
    w = 0.1
    y1 = -0.84
    y2 = y1 + 2*w
    y3 = y1 + 6*w
    y4 = 0.6
    y5 = -y1
    x1 = (y1 + w) * 0.618 - w
    x2 = x1 / 2
    x3 = 0
    x4 = x5 / 2
    x5 = -x1
    α  = pi / 3
    ct = 1 / tan α 
    dirs = mkPath $ do
      m  (x3 - w)  (y3 + w)
      l  (x1 - w)  (y3 + w)
      l  (x1 - w)  (y3 - w)
      l  (x3 - w)  (y3 - w)
      l  (x3 - w)  (y2 + w)
      l  (x2 - w)  (y2 + w)
      l  (x2 - w)  (y2 - w)
      l  (x3 - w)  (y2 - w)
      l  (x3 - w)  (y1 - w)
      l  (x3 + w)  (y1 - w)
      l  (x3 + w)  (y2 - w)
      l  (x4 + w)  (y2 - w)
      l  (x4 + w)  (y2 + w)
      l  (x3 + w)  (y2 + w)
      l  (x3 + w)  (y3 - w)
      l  (x5 + w)  (y3 - w)
      l  (x5 + w)  (y3 + w)
      l  (x3 + w)  (y3 + w)
      l  (x3 + w)  (y4 - w + w  * ct)
      l  (x4 + w)  (y4 - w + (x4 + w) * ct)
      l  (x4 + w)  (y4 + w + (x4 + w) * ct)
      l  (x3 + w)  (y4 + w + w  * ct)
      l  (x3 + w)  (y5 + w)
      l  (x3 - w)  (y5 + w)
      l  (x3 - w)  (y4 + w - w  * ct)
      l  (x2 - w)  (y4 + w - (x4 + w) * ct)
      l  (x2 - w)  (y4 - w - (x4 + w) * ct)
      l  (x3 - w)  (y4 - w - w  * ct)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crescentAndStar_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crescentAndStar_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/crescentAndStar_strk.svg)
-}
crescentAndStar :: Svg
crescentAndStar =
    S.g $ do
      S.path
        ! A.strokeLinejoin "round"
        ! A.d moonDirs
      starRegular 5 0.3 (0.55, 0.05)
  where
    kx = 0.55
    ky = 0.55
    r1 = 0.8
    r2 = 0.65
    moonDirs = mkPath $ do
      m   ( kx) (-ky)
      aa    r1    r1   0  True  False ( kx) ( ky)
      aa    r2    r2   0  True  True  ( kx) (-ky)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/starOfDavid_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/starOfDavid_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/starOfDavid_strk.svg)
-}
starOfDavid :: Svg
starOfDavid =
  starPolygonFirstSpecies 6 0.9 (0,0)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/exampleHexagram_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/exampleHexagram_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/religion/exampleHexagram_strk.svg)

Function to draw a hexagram from the Yi Ching (the Book of Mutations).
If all six numbers belong to {0,1} it draws only the hexagram lines.
Otherwise, the numbers are printed right to their line
-}
iChingHexagram :: (Int,Int,Int,Int,Int,Int) -> Svg
iChingHexagram (n1,n2,n3,n4,n5,n6) =
    S.g $ do
      S.path
        ! A.d lines
      if doNotPrintNumbers
        then mempty
        else numbers
  where
    doNotPrintNumbers =
      (\k -> k == 0 || k == 1) `all` [n1,n2,n3,n4,n5,n6]
    x1 = 0.7
    x2 = 0.1
    ky = 2 / 14
    line k y =
      if (odd k)
        then   m (-x1) y >> l x1 y
        else   m (-x1) y >> l (-x2) y >> m x2 y >> l x1 y
    lines = mkPath $ do
      line n6 (-5*ky)
      line n5 (-3*ky)
      line n4 (-1*ky)
      line n3 ( 1*ky)
      line n2 ( 3*ky)
      line n1 ( 5*ky)
    number k y =
      S.text_ (fromString $ show k)
        ! (A.x .: 0.85)
        ! (A.y .: y)
        ! A.dominantBaseline "central"
        ! A.textAnchor  "middle"
        ! A.fontFamily  "Times New Roman, serif"
        ! A.fontSize    "0.2"
        ! A.strokeWidth "0"
    numbers = 
      S.g $ do
        number n6 (-5*ky)
        number n5 (-3*ky)
        number n4 (-1*ky)
        number n3 ( 1*ky)
        number n2 ( 3*ky)
        number n1 ( 5*ky)
