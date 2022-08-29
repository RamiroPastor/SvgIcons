{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Religion where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgReligion :: [ (String , S.Svg) ]
svgReligion =
  [ (,) "xp"       xp
  , (,) "taijitu" (taijitu "black" "white")
  , (,) "cross"    cross
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

  
cross :: Svg
cross =
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

