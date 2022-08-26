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
  , (,) "archive"  archive
  , (,) "pin"      pin
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


archive :: S.Svg
archive = 
  S.g $ do
    S.path
      ! A.strokeLinejoin "round"
      ! A.d archiveBody
    archiveHandle (-ky * 2/3)
    archiveHandle  0
    archiveHandle ( ky * 2/3)
  where
    ky = 0.96
    kx = 0.75
    archiveBody = mkPath $ do
      m   (-kx)  (-ky)
      l   (-kx)  ( ky)
      l   ( kx)  ( ky)
      l   ( kx)  (-ky)
      S.z
      m   (-kx)  (-1/3 * ky)
      l   ( kx)  (-1/3 * ky)
      m   (-kx)  ( 1/3 * ky)
      l   ( kx)  ( 1/3 * ky)
    archiveHandle h =
      S.path
        ! A.fill "none"
        ! A.strokeLinecap "round"
        ! A.transform (translate 0 (hr/2))
        ! A.d (handleDirs h)
    hx = 0.25
    hr = 0.07
    handleDirs h = mkPath $ do
      m   (-hx)      (h - hr)
      aa  hr  hr  0  False False (-hx + hr) (h)
      l   ( hx - hr) (h)
      aa  hr  hr  0  False False ( hx)      (h - hr)

  
pin :: Svg
pin =
  S.g $
    S.path
      ! A.strokeLinejoin "arcs"
      ! A.strokeMiterlimit "8"
      ! A.d (mkPath $ topPath >> bodyPath >> needlePath)
      ! A.transform (S.rotateAround 45 0 0)
  where
    w1 = 0.26
    w2 = 0.08
    y1 = -0.95
    y2 = -0.7
    y3 = -0.1
    y4 =  0.16
    y5 =  0.6
    y6 =  1.03
    r1 = (y2 - y1) / 2
    r2 = (y4 - y3)
    topPath = do
      m   (-w1)  y1
      aa  r1 r1 0 True False (-w1) y2
      l   ( w1)  y2
      aa  r1 r1 0 True False ( w1) y1
      l   (-w1)  y1
    bodyPath = do
      m   (-w1)  y2
      l   (-w1)  y3
      aa  r2 r2 0 False False (-w1 - r2) y4
      l   ( w1 + r2) y4
      aa  r2 r2 0 False False ( w1)      y3
      l   ( w1)  y2
    needlePath = do
      m   (-w2)  y4
      l   (-w2)  y5
      l     0    y6
      l   ( w2)  y5
      l   ( w2)  y4