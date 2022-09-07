{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Religion where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base
import Geometry



svgReligion :: [ (String , S.Svg) ]
svgReligion =
  [ (,) "xp"               xp
  , (,) "taijitu"         (taijitu "black" "white")
  , (,) "crossLatin"       crossLatin
  , (,) "crossOrthodox"    crossOrthodox
  , (,) "crescentAndStar"  crescentAndStar
  , (,) "starOfDavid"      starOfDavid
  ]


--------------------------------------------------------------------------------


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


crescentAndStar :: Svg
crescentAndStar =
    S.g $ do
      S.path
        ! A.strokeLinejoin "round"
        ! A.d moonDirs
      starRegular 5 0.3 (0.56, 0.05)
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

  
starOfDavid :: Svg
starOfDavid =
  S.g 
    ! A.transform (rotateAround 30 0 0)
    $ do
      starPolygonFirstSpecies 6 0.9 (0,0)
      -- starPolygonFirstSpecies 6 0.7 (0,0)