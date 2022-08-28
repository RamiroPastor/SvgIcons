{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Religion where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgReligion :: [ (String , S.Svg) ]
svgReligion =
  [ (,) "xp" xp
  ]


--------------------------------------------------------------------------------


xp :: Svg
xp = 
  S.g $ do
    S.path
      ! A.d rho
    S.path 
      ! A.d chi
      ! A.transform (rotateAround 45 0 0)
    frame (-1) (-1) 2 2
  where
    w = 0.07
    k1 = 0.5
    k2 = 0.96
    r1 = 0.17
    r2 = r1 - w
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
      l   (    w)  ( k2-2*r1)
      S.z