{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Office 
  ( svgOffice
  , archive
  , briefcase
  , clipboard
  , document
  , envelope
  , lupe
  , paperclip
  , pencil
  , pin
  , printer
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils



{- |
A list with all the icons of this module, 
together with appropriate names.

>svgOffice :: [ (String , S.Svg) ]
>svgOffice =
>  [ (,) "archive"    archive
>  , (,) "briefcase"  briefcase
>  , (,) "clipboard"  clipboard
>  , (,) "document"   document
>  , (,) "envelope"   envelope
>  , (,) "lupe"       lupe
>  , (,) "paperclip"  paperclip
>  , (,) "pencil"     pencil
>  , (,) "pin"        pin
>  , (,) "printer"    printer
>  ]
-}
svgOffice :: [ (String , S.Svg) ]
svgOffice =
  [ (,) "archive"    archive
  , (,) "briefcase"  briefcase
  , (,) "clipboard"  clipboard
  , (,) "document"   document
  , (,) "envelope"   envelope
  , (,) "lupe"       lupe
  , (,) "paperclip"  paperclip
  , (,) "pencil"     pencil
  , (,) "pin"        pin
  , (,) "printer"    printer
  ]


--------------------------------------------------------------------------------



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/envelope_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/envelope_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/envelope_strk.svg)
-}
envelope :: Svg
envelope =
    S.path
      ! d dirs
      ! strokeLinejoin "round"
  where
    w = 0.95
    h = 0.618 * w   
    ky = 0.16    -- y-coordinate of the middle point
    kx = 0.2
    (a1,a2) = (,)  (-w)  (-h)  -- top left corner
    (b1,b2) = (,)  (-w)  ( h)  -- bottom left corner
    (c1,c2) = (,)  ( w)  ( h)  -- bottom right corner
    (d1,d2) = (,)  ( w)  (-h)  -- top right corner
    (e1,e2) = (,)    0    ky   -- middle point
    (f1,f2) = (,)  (-kx)   0
    (g1,g2) = (,)  ( kx)   0
    dirs = mkPath $ do
      m   a1 a2
      l   b1 b2
      l   c1 c2
      l   d1 d2
      l   a1 a2
      l   e1 e2
      l   d1 d2
      m   b1 b2
      l   f1 f2
      m   c1 c2
      l   g1 g2
      


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/pencil_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/pencil_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/pencil_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/document_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/document_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/document_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/archive_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/archive_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/archive_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/pin_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/pin_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/pin_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/paperclip_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/paperclip_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/paperclip_strk.svg)
-}
paperclip :: Svg
paperclip =
    S.g $ 
      S.path 
        ! A.d clipDirs
        ! A.transform (rotateAround 45 0 0)
  where
    w  = 0.07
    x1 =  0.2
    x2 =  0.44
    r1 =  x1
    r2 = (x1 + x2) / 2
    r3 =  x2
    y1 =  0.5
    y2 = -0.48
    y3 =  0.68
    y4 = -0.66
    clipDirs = mkPath $ do
      m  (-x1 - w)  y1
      l  (-x1 - w)  y2
      aa ( r1 + w)  ( r1 + w)   0   True  True  ( x1 + w)  y2
      l  ( x1 + w)  y3
      aa ( r2 + w)  ( r2 + w)   0   True  True  (-x2 - w)  y3
      l  (-x2 - w)  y4
      aa ( r3 + w)  ( r3 + w)   0   True  True  ( x2 + w)  y4
      l  ( x2 + w)  y1
      aa (      w)  (      w)   0   True  True  ( x2 - w)  y1
      l  ( x2 - w)  y4
      aa ( r3 - w)  ( r3 - w)   0   True  False (-x2 + w)  y4
      l  (-x2 + w)  y3
      aa ( r2 - w)  ( r2 - w)   0   True  False ( x1 - w)  y3
      l  ( x1 - w)  y2
      aa ( r1 - w)  ( r1 - w)   0   True  False (-x1 + w)  y2
      l  (-x1 + w)  y1
      aa (      w)  (      w)   0   True  True  (-x1 - w)  y1
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/clipboard_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/clipboard_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/clipboard_strk.svg)
-}
clipboard :: Svg
clipboard =
  S.g $ do
    S.path
      ! d boardDirs
    S.path
      ! d clipDirs
  where
    dx1 = 0.3
    dx2 = 0.7
    y1 = -0.9
    y2 = -0.7
    y3 = -0.5
    y4 =  0.9
    r1 =  0.2
    r2 =  0.24
    boardDirs = mkPath $ do
      m   (-dx1     ) (y2     )
      l   (-dx2 + r2) (y2     )
      aa  r2  r2  0  False False (-dx2     ) (y2 + r2)
      l   (-dx2     ) (y4 - r2)
      aa  r2  r2  0  False False (-dx2 + r2) (y4     )
      l   ( dx2 - r2) (y4     )
      aa  r2  r2  0  False False ( dx2     ) (y4 - r2)
      l   ( dx2     ) (y2 + r2)
      aa  r2  r2  0  False False ( dx2 - r2) (y2     )
      l   ( dx1     ) (y2     )
    clipDirs = mkPath $ do
      m   (-dx1     ) (y2     )
      l   (-dx1     ) (y3     )
      l   ( dx1     ) (y3     )
      l   ( dx1     ) (y1 + r1)
      aa  r1  r1  0  False False ( dx1 - r1) (y1     )
      l   (-dx1 + r1) (y1     )
      aa  r1  r1  0  False False (-dx1     ) (y1 + r1)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/printer_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/printer_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/printer_strk.svg)
-}
printer :: Svg
printer =
    S.g $ do
      S.path
        ! A.d topDirs
      S.path
        ! A.d midDirs
      S.path 
        ! A.strokeLinecap "round"
        ! A.d botDirs
      dots
  where
    x0 =  0.5
    x1 =  0.9
    x2 =  x0 - 0.1
    y0 =  0.4
    y1 =  0.8
    y2 =  0.2
    y3 =  0.94
    ky = (y3 - y2)/4
    r0 =  0.1
    dx =  0.3
    dy = -0.1
    dr =  0.09
    topDirs = mkPath $ do
      m   (-x0)  (-y0)
      l   (-x0)  (-y1)
      l   ( x0)  (-y1)
      l   ( x0)  (-y0)
    midDirs = mkPath $ do
      m   (-x0)      ( y0)
      l   (-x1 + r0) ( y0)
      aa  r0  r0  0  False True  (-x1)      ( y0 - r0)
      l   (-x1)      (-y0 + r0)
      aa  r0  r0  0  False True  (-x1 + r0) (-y0)
      l   ( x1 - r0) (-y0)
      aa  r0  r0  0  False True  ( x1)      (-y0 + r0)
      l   ( x1)      ( y0 - r0)
      aa  r0  r0  0  False True  ( x1 - r0) ( y0)
      l   ( x0)      ( y0)
    botDirs = mkPath $ do
      m   (-x0)  y2
      l   ( x0)  y2
      l   ( x0)  y3
      l   (-x0)  y3
      S.z
      m   (-x2)  (y2 +   ky)
      l   ( x2)  (y2 +   ky)
      m   (-x2)  (y2 + 2*ky)
      l   ( x2)  (y2 + 2*ky)
      m   (-x2)  (y2 + 3*ky)
      l   ( x2)  (y2 + 3*ky)
    dots =
      do
        S.circle
          ! A.stroke "none"
          ! (A.cx .: (-dx))
          ! (A.cy .: dy)
          ! (A.r  .: dr)
          ! A.fill "hotpink"
        S.circle
          ! A.stroke "none"
          ! (A.cx .: 0 )
          ! (A.cy .: dy)
          ! (A.r  .: dr)
          ! A.fill "lime"
        S.circle
          ! A.stroke "none"
          ! (A.cx .: dx)
          ! (A.cy .: dy)
          ! (A.r  .: dr)
          ! A.fill "deepskyblue"



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/lupe_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/lupe_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/lupe_strk.svg)
-}
lupe :: S.Svg
lupe =
    S.g $ do
      S.path
        ! A.d dirs
        ! A.fillRule "evenodd"
        ! A.transform (rotateAround 45 0 0)
  where
    w1 = 0.08
    w2 = 0.12
    r1 = 0.54
    r2 = r1 + 2*w1
    x1 = 0.07
    x2 = w2
    y1 = 0.3
    y2 = 0.6
    y3 = 1.12
    dirs = mkPath $ do
      m     0   (y1 - w1)
      aa    r1   r1   0  True  False (-0.01) ( y1 - w1)
      S.z
      m   (-x1) (y1 + w1)
      aa    r2   r2   0  True  True  ( x1)   ( y1 + w1)
      q     x2  (y2 - 0.18)   x2   y2
      l     x2   y3
      aa    w2   w2   0  True  True  (-x2)   ( y3)
      l   (-x2)  y2
      q   (-x2) (y2 - 0.18) (-x1)  (y1 + w1)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/briefcase_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/briefcase_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/office/briefcase_strk.svg)
-}
briefcase :: Svg
briefcase = 
    S.g $ do
      S.path
        ! A.strokeLinejoin "round"
        ! A.d top
      S.path
        ! A.strokeLinejoin "round"
        ! A.d bot
      button
  where
    r1 =  0.2
    r2 =  0.07
    x1 =  0.94
    x2 =  0.46
    x3 =  0.3
    y1 = -0.86
    y2 = -0.7
    y3 = -0.54
    y4 =  0
    y5 =  0.1
    y6 =  0.8
    rx1 = 0.16
    ry1 = 0.22
    rx2 = 0.07
    ry2 = 0.135
    top = mkPath $ do
      m     x3        y3
      l     x3        y2
      l   (-x3)       y2
      l   (-x3)       y3 
      S.z
      m   (-x1)       y4
      l   (-x1)      (y3 + r1)
      aa    r1        r1   0   False True  (-x1 + r1)  y3
      l   (-x2)       y3
      l   (-x2)      (y1 + r2)
      aa    r2        r2   0   False True  (-x2 + r2)  y1
      l   ( x2 - r2)  y1
      aa    r2        r2   0   False True  ( x2)      (y1 + r2)
      l     x2        y3
      l   ( x1 - r1)  y3
      aa    r1        r1   0   False True  ( x1)      (y3 + r1)
      l     x1        y4
      l     rx1       y4
      aa    rx1       ry1  0   False False (-rx1)      y4
      S.z
    bot = mkPath $ do
      m   (-x1)       y5
      l   (-x1)      (y6 - r1)
      aa    r1        r1   0   False False (-x1 + r1)  y6
      l   ( x1 - r1)  y6
      aa    r1        r1   0   False False   x1       (y6 - r1)
      l     x1        y5
      l     rx1       y5
      aa    rx1       ry1  0   False True  (-rx1)      y5
      S.z
    button = 
      S.ellipse
        ! (A.cx .: 0)
        ! (A.cy .: (y5 + y4)/2)
        ! (A.rx .: rx2)
        ! (A.ry .: ry2)
