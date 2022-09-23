{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Textarea 
  ( svgTextarea
  , bold
  , italic
  , link
  , imageIcon
  , video
  , horizontalBars
  , bulletList
  , numberList
  , header
  , horizontalRule
  , curvyArrowLeft
  , undo
  , redo
  , questionMark
  , fullscreen
  , preview
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils


{- |
A list with all the icons of this module, 
together with appropriate names.
This module contains icons suitable for the 
tool bars of a text editor (bold, italic, etc.)

>svgTextarea :: [ (String , S.Svg) ]
>svgTextarea =
>  [ (,) "bold"       bold
>  , (,) "italic"     italic
>  , (,) "link"       link
>  , (,) "image"      imageIcon
>  , (,) "video"      video
>  , (,) "bulletList" bulletList
>  , (,) "numberList" numberList
>  , (,) "header"     header
>  , (,) "hr"         horizontalRule
>  , (,) "undo"       undo
>  , (,) "redo"       redo
>  , (,) "help"       questionMark
>  , (,) "fullscreen" fullscreen
>  , (,) "preview"    preview
>  ]
-}
svgTextarea :: [ (String , S.Svg) ]
svgTextarea =
  [ (,) "bold"       bold
  , (,) "italic"     italic
  , (,) "link"       link
  , (,) "image"      imageIcon
  , (,) "video"      video
  , (,) "bulletList" bulletList
  , (,) "numberList" numberList
  , (,) "header"     header
  , (,) "hr"         horizontalRule
  , (,) "undo"       undo
  , (,) "redo"       redo
  , (,) "help"       questionMark
  , (,) "fullscreen" fullscreen
  , (,) "preview"    preview
  ]


--------------------------------------------------------------------------------



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/bold_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/bold_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/bold_strk.svg)
-}
bold :: S.Svg
bold =
    S.path 
      ! d dirs
  where
    k0 = 0.2
    k1 = (-0.15)
    k2 = 0.42
    k3 = 2 * k2
    k6 = 0.2
    dirs = mkPath $ do
      m  (-1 + k0) (-1 + k0)
      l   k1       (-1 + k0)
      l   k1       (-1 + k0)
      l   k6       (-1 + k0)
      aa 0.25 0.25 0 True True  k6         0
      l                        (k6 + 0.1)  0
      aa 0.4  0.35 0 True True (k6 + 0.1) (1 - k0)
      l   k1       ( 1 - k0)
      l  (-1 + k0) ( 1 - k0)
      l  (-1 + k0) ( 1 - k2)
      q  (-1 + k2) ( 1 - k2) (-1 + k2) ( 1 - k3)
      l  (-1 + k2) (-1 + k3)
      q  (-1 + k2) (-1 + k2) (-1 + k0) (-1 + k2)
      l  (-1 + k0) (-1 + k0)
      S.z
      m                         k1          (-(1 - k0)/2 + 0.15)
      aa 0.22 0.22 0 True False k1          (-(1 - k0)/2 - 0.15)
      S.z
      m                         k1          0.55
      l                         (k6 - 0.1)  0.55
      aa 0.20 0.15 0 True False (k6 - 0.1)  0.2
      l                         k1          0.2
      S.z
      


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/italic_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/italic_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/italic_strk.svg)
-}
italic :: S.Svg
italic =
    S.path
      ! d dirs
  where
    k1 = 0.12  -- half width of the line
    k2 = 1     -- length of the horizontal lines
    k3 = 0.2   -- distance from the center of the horizontal lines to y-axis
    k4 = 0.65  -- distance from the center of the horizontal lines to x-axis
    x1  = -k3 - 0.5 * k2
    x1' = -k3 - k1
    x2' = -k3 + k1
    x2  = -k3 + 0.5 * k2
    x3  =  k3 - 0.5 * k2
    x3' =  k3 - k1
    x4' =  k3 + k1
    x4  =  k3 + 0.5 * k2
    y1  =  k4 + k1
    y2  =  k4 - k1
    y3  = -k4 + k1
    y4  = -k4 - k1
    dirs = mkPath $ do
      m   x1  y1
      l   x2  y1
      aa  k1  k1  0  True  False x2  y2
      l   x2' y2
      l   x4' y3 
      l   x4  y3
      aa  k1  k1  0  True  False x4  y4
      l   x3  y4
      aa  k1  k1  0  True  False x3  y3
      l   x3' y3
      l   x1' y2
      l   x1  y2
      aa  k1  k1  0  True  False x1  y1
      S.z 



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/link_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/link_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/link_strk.svg)
-}
link :: S.Svg
link =
    g $ do
      topPart ! A.transform (rotateAround 45 0 0)
      topPart ! A.transform (rotateAround 45 0 0 <> rotateAround 180 0 0)
  where
    topPart = S.path ! d topPath 
    -----------------
    w1 = 0.4
    w2 = 0.24
    -----------------
    h1 = 1 - w2
    h2 = 0.4
    h3 = 0.3
    h4 = h3 - (w1 - w2)
    topPath = mkPath $ do
      m   (-w1)  (-h3)
      l   (-w1)  (-h1)
      aa  w1 w1 0 True  True  ( w1) (-h1)
      l   ( w1)  (-h4)
      aa  w1 w1 0 False True    0   ( h3)
      aa  ((h3 - h4)/2) ((h3 - h4)/2) 0 False True 0 ( h4)
      aa  w2 w2 0 False False ( w2) (-h4)
      l   ( w2)  (-h1)
      aa  w2 w2 0 True False (-w2) (-h1)
      l   (-w2)  (-h2)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/image_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/image_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/image_strk.svg)
-}
imageIcon :: S.Svg
imageIcon =
    S.g $ do
      sun
      mountain
      imageFrameStroked
      imageFrameFilled
  where
    sun =
      circle
        ! (cx .: 0.5)
        ! (cy .: (-0.5))
        ! (r  .: 0.24)
    -------------------------
    x = 0.09
    imageFrameFilled =
      S.path
        ! d framePath1
        ! A.stroke "none"
    framePath1 = mkPath $ do
      m (-1) (-1)
      l   1  (-1)
      l   1    1
      l (-1)   1
      S.z
      m (-1 + x) (-1 + x)
      l (-1 + x) ( 1 - x)
      l ( 1 - x) ( 1 - x)
      l ( 1 - x) (-1 + x)
      S.z
    imageFrameStroked =
      S.path
        ! A.fill "none"
        ! d framePath2
    framePath2 = mkPath $ do
      m (-0.94) (-0.94)
      l   0.94  (-0.94)
      l   0.94    0.94
      l (-0.94)   0.94
      S.z
    -------------------------
    mountain =
      S.path
        ! A.d mountainPath
    mountainPath = mkPath $ do
      m   (-0.92)   0.92
      l   (-0.35) (-0.35)
      l     0       0.55
      l     0.45    0.2
      l     0.92    0.92
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/video_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/video_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/video_strk.svg)
-}
video :: S.Svg
video =
    S.path ! A.d boxPath
  where
    h  = 1.2
    w  = 1.618 * h
    y1 = -0.5 * h
    y2 =  0.5 * h
    x1 = -0.5 * w
    x2 =  0.5 * w
    tx =  0.16
    th =  3 * tx
    boxPath = mkPath $ do
      m   x1  0
      c   x1 (y1 + 0.1 ) x1 (y1 + 0  )  0  y1
      c   x2 (y1 + 0.0 ) x2 (y1 + 0.1) x2   0
      c   x2 (y2 - 0.1 ) x2 (y2 + 0  )  0  y2
      c   x1 (y2 + 0.0 ) x1 (y2 - 0.1) x1   0
      S.z
      m  (0 - tx) (0 - th/2)
      l  (0 - tx) (0 + th/2)
      l  (2*th/3)  0
      S.z



{- |
Helper for both list icons
-}
horizontalBars :: S.Svg
horizontalBars =
    S.g $ do
      S.path ! d topLine
      S.path ! d midLine
      S.path ! d botLine
  where
    w  =  0.20
    x1 = -0.4
    x2 =  0.92
    y1 = -0.6
    y2 =  0
    y3 =  0.6
    topLine = mkPath $ do
      m x1 (y1 - w/2)
      l x2 (y1 - w/2)
      l x2 (y1 + w/2)
      l x1 (y1 + w/2)
      S.z
    midLine = mkPath $ do
      m x1 (y2 - w/2)
      l x2 (y2 - w/2)
      l x2 (y2 + w/2)
      l x1 (y2 + w/2)
      S.z
    botLine = mkPath $ do
      m x1 (y3 - w/2)
      l x2 (y3 - w/2)
      l x2 (y3 + w/2)
      l x1 (y3 + w/2)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/bulletList_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/bulletList_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/bulletList_strk.svg)
-}
bulletList :: S.Svg
bulletList =
    S.g $ do
      horizontalBars
      bullets 
  where
    radius = 0.12
    x1 = -0.75
    y1 = -0.6
    y2 =  0
    y3 =  0.6
    bullets = S.g $ do
      circle ! (cx .: x1) ! (cy .: y1) ! (r .: radius) 
      circle ! (cx .: x1) ! (cy .: y2) ! (r .: radius) 
      circle ! (cx .: x1) ! (cy .: y3) ! (r .: radius) 



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/numberList_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/numberList_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/numberList_strk.svg)
-}
numberList :: Svg
numberList =
    S.g $ do
      horizontalBars
      numbers
  where
    x1 = -0.75
    y1 = -0.6
    y2 =  0
    y3 =  0.6
    number k h = 
      S.text_ k
        ! dominantBaseline "central"
        ! textAnchor "middle"
        ! fontFamily "monospace"
        ! fontWeight "bold"
        ! fontSize   "0.5"
        ! (A.x .: x1)
        ! (A.y .: h)
    numbers = 
      S.g $ do
        number "1" y1
        number "2" y2
        number "3" y3



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/header_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/header_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/header_strk.svg)
-}
header :: S.Svg
header =
    S.g $ do
      S.path ! d line1
      S.path ! d line2
      S.path ! d line3 ! opacity "0.4"
      S.path ! d line4 ! opacity "0.4"
      S.path ! d line5 ! opacity "0.4"
  where
    l1 = -0.9
    l2 = -0.5
    r2 =  0.5
    r1 =  0.9
    h = 2/6
    w = 0.2
    line1 = mkPath $ do
       m l1 (-2*h - w/2)
       l r1 (-2*h - w/2)
       l r1 (-2*h + w/2)
       l l1 (-2*h + w/2)
       S.z
    line2 = mkPath $ do
       m l2 (-1*h - w/2)
       l r2 (-1*h - w/2)
       l r2 (-1*h + w/2)
       l l2 (-1*h + w/2)
       S.z
    line3 = mkPath $ do
       m l1 (   0 - w/2)
       l r1 (   0 - w/2)
       l r1 (   0 + w/2)
       l l1 (   0 + w/2)
       S.z
    line4 = mkPath $ do
       m l2 ( 1*h - w/2)
       l r2 ( 1*h - w/2)
       l r2 ( 1*h + w/2)
       l l2 ( 1*h + w/2)
       S.z
    line5 = mkPath $ do
       m l1 ( 2*h - w/2)
       l r1 ( 2*h - w/2)
       l r1 ( 2*h + w/2)
       l l1 ( 2*h + w/2)
       S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/hr_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/hr_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/hr_strk.svg)
-}
horizontalRule :: S.Svg
horizontalRule =
    S.g $ do
      S.path ! d line1 ! opacity "0.4"
      S.path ! d line2 ! opacity "0.4"
      line3 
      S.path ! d line4 ! opacity "0.4"
      S.path ! d line5 ! opacity "0.4"
  where
    l1 = -0.9
    l2 = -0.5
    r2 =  0.5
    r1 =  0.9
    h = 2/6
    w = 0.2
    line1 = mkPath $ do
       m l1 (-2*h - w/2)
       l r1 (-2*h - w/2)
       l r1 (-2*h + w/2)
       l l1 (-2*h + w/2)
       S.z
    line2 = mkPath $ do
       m l2 (-1*h - w/2)
       l r2 (-1*h - w/2)
       l r2 (-1*h + w/2)
       l l2 (-1*h + w/2)
       S.z
    square leftX =
      S.rect
        ! (A.x    .: leftX)
        ! (A.y    .: 0 - w/2)
        ! (width  .: w)
        ! (height .: w)
    line3 = do
      square l1
      square l2
      square ( 0 - w/2)
      square (r2 - w)
      square (r1 - w)
    line4 = mkPath $ do
       m l2 ( 1*h - w/2)
       l r2 ( 1*h - w/2)
       l r2 ( 1*h + w/2)
       l l2 ( 1*h + w/2)
       S.z
    line5 = mkPath $ do
       m l1 ( 2*h - w/2)
       l r1 ( 2*h - w/2)
       l r1 ( 2*h + w/2)
       l l1 ( 2*h + w/2)
       S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/undo_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/undo_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/undo_strk.svg)
-}
undo :: S.Svg
undo =
  curvyArrowLeft
    ! A.transform (translate 0 0.1)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/redo_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/redo_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/redo_strk.svg)
-}
redo :: S.Svg
redo =
  curvyArrowLeft
    ! A.transform (translate 0 0.1 <> horizontalMirrorMatrix)



{- |
Helper for both undo and redo icons
-}
curvyArrowLeft :: S.Svg
curvyArrowLeft =
  S.path
    ! d dirs
    ! strokeLinejoin "round"
  where
    r1 = 0.5
    r2 = 0.66
    rm = (r2 - r1)
    k1 = 0.24
    k2 = k1 + rm/2
    dirs = mkPath $ do
      m   (-r1) 0
      aa  (rm/2) (rm/2)  0 False False (-r2)   0
      aa   r2     r2     0 True  False   0   (-r2)
      lr    k1  (-k1)
      lr  (-rm)   0
      lr  (-k2)   k2
      lr    k2    k2
      lr    rm    0
      lr  (-k1) (-k1)
      aa   r1     r1     0 True  True  (-r1)   0
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/help_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/help_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/help_strk.svg)
-}
questionMark :: S.Svg
questionMark =
    S.path
      ! A.d dirs
      ! A.transform (translate 0 (-0.35))
  where
    r1 = 0.3
    r2 = 0.5
    rm = r2 - r1
    dirs = mkPath $ do
      m   (-r1)    0
      aa  ( rm/2) (rm/2)  0   True  True  (-r2)  0
      aa  ( r2)   (r2)    0   True  True  ( r2)  0
      c   ( r2)   (r2 - 0.17)  (rm/2) (r2 - 0.2) (rm/2) 0.5
      l   ( rm/2) 0.75
      aa  ( rm/2) (rm/2)  0   True  True  (-rm/2)  0.75
      l   (-rm/2) 0.5
      c   (-rm/2) (r1 - 0.15)  (r1)   (r1 - 0.1) (r1)   0  
      aa  ( r1)   (r1)    0   True  False (-r1)  0
      S.z
      m   (-0.01)  1
      aa  ( rm/2) (rm/2)  0  True False  0.01 1
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/fullscreen_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/fullscreen_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/fullscreen_strk.svg)
-}
fullscreen :: S.Svg
fullscreen =
    S.g $ do
      corner
      corner ! A.transform (rotateAround  90 0 0)
      corner ! A.transform (rotateAround 180 0 0)
      corner ! A.transform (rotateAround 270 0 0)
 where
   k1 = 0.9
   k2 = 0.7
   k3 = 0.3
   km = k1 - k2
   corner =
     S.path
      ! d dirs
      ! strokeLinejoin "round"
   dirs = mkPath $ do
      m   k1     k1
      l   k3     k1
      aa (km/2) (km/2)  0  True  True  k3  k2
      l   k2     k2
      l   k2     k3
      aa (km/2) (km/2)  0  True  True  k1  k3
      S.z
     


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/preview_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/preview_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/textarea/preview_strk.svg)
-}
preview :: S.Svg
preview = 
    S.g $ do
      lines
      rectangle
  where
    kx = 0.2
    ky = 0.4
    w  = 0.16
    lines = 
      S.g $ do
        line (-2 * ky)
        line (-1 * ky)
        line ( 0 * ky)
        line ( 1 * ky)
        line ( 2 * ky)
    line y =
      S.path
        ! fill "none"
        ! d (lineDirs y)
    lineDirs y = mkPath $ do
      m   (-1 + kx) (y - w/2)
      aa  (w/2) (w/2) 0 True  False (-1 + kx) (y + w/2) 
      l   ( 0     ) (y + w/2)
      aa  (w/2) (w/2) 0 True  False ( 0     ) (y - w/2)
      S.z
    rectangle =
      S.path 
        ! strokeLinejoin "round"
        ! transform (translate 0.1 0)
        ! d rectDirs
    rectDirs = mkPath $ do
      m   (0 + kx)  (-2 * ky - w/2)
      l   (1 - kx)  (-2 * ky - w/2)
      l   (1 - kx)  ( 2 * ky + w/2)
      l   (0 + kx)  ( 2 * ky + w/2)
      S.z