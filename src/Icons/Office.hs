{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Office where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgOffice :: [ (String , S.Svg) ]
svgOffice =
  [ (,) "envelope" envelope
  , (,) "pencil"   pencil
  ]


--------------------------------------------------------------------------------





envelope :: Svg
envelope =
    S.path
      ! d dirs
      ! strokeLinejoin "round"
  where
    kx = 0.94
    ky = 0.618 * kx
    mx = 0
    my = 0
    s  = 0.03
    dirs = mkPath $ do
      m   (-kx)  (-ky)
      l   ( mx)      ( my)
      l   ( kx)  (-ky)
      S.z
      m   (-kx)      (-ky + s)
      l   (-kx)      ( ky)
      l   ( kx)      ( ky)
      l   ( kx)      (-ky + s)
      l   ( mx)      ( my + s)
      S.z
      

pencil :: Svg
pencil =
    S.path
      ! d pencilPath
      ! strokeLinejoin "round"
  where
    w  = 0.6   -- width of the pencil
    x1 = -w/2
    x2 =  w/2
    y1 = -0.9
    y2 = -0.76
    y3 = -0.6
    y4 =  0.4
    y5 =  y6 - 0.15
    y6 =  0.94
    pencilPath = mkPath $ do
      m   0   y5
      l   0   y6
      l   x1  y4
      l   x2  y4
      l   0   y6
      m   x1  y4
      l   x1  y3
      l   x2  y3
      l   x2  y4
      m   0   y4
      l   0   y3
      m   x1  y3
      l   x1  y2
      l   x2  y2
      l   x2  y3
      m   x1  y2
      l   x1  y1
      aa  (w/2) 0.03 0 True True x2 y1
      l   x2  y2
      