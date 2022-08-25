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
  , (,) "document" document
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


document :: Svg
document = 
    S.g $ do
      paperBorder
      S.path
        ! strokeLinecap "round"
        ! d lines
      S.path
        ! A.d xMark
        ! fill "none"
        ! strokeLinecap "round"
        ! transform (translate (-0.25) 0.55 <> rotateAround 45 0 0)
      pencil 
        ! transform (translate   0.55  0.35 <> rotateAround 45 0 0 <> S.scale 0.4 0.4)
  where
    py = 0.95
    px = 0.618 * py
    paperBorder =
      S.path
        ! strokeLinejoin "round"
        ! strokeLinecap  "round"
        ! fill "none"
        ! d paperPath
    paperPath = mkPath $ do
      m   ( px)  (-0.16)
      l   ( px)  (-py)
      l   (-px)  (-py)
      l   (-px)  ( py)
      l   ( px)  ( py)
      l   ( px)  ( 0.7)
    --------------------------------------------------
    xl =  0.4
    y1 = -0.65
    y2 = -0.35
    y3 = -0.05
    lines = mkPath $ do
      m  (-xl) y1   >>   l  xl  y1
      m  (-xl) y2   >>   l  xl  y2
      m  (-xl) y3   >>   l  xl  y3  
    --------------------------------------------------
    m1 = 0.15
    xMark = mkPath $ do
      m  (-m1)   0
      l  ( m1)   0
      m    0   (-m1)
      l    0   ( m1)