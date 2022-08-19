{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Textarea where

import           Data.Default
import           Data.List (intercalate)
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base
import Core


svgTextarea :: [ (String , S.Svg) ]
svgTextarea =
  [ (,) "bold"    $ bold def{strokeSize = 0, fillColor = "black"}
  , (,) "italic"  italic
  , (,) "link"    link
  , (,) "image"   imageIcon
  , (,) "video"   video
  , (,) "bulletL" bulletList
  , (,) "numberL" numberList
  , (,) "header"  header
  , (,) "hr"      horizontalRule
  , (,) "undo"    undo
  , (,) "redo"    redo
  , (,) "help"    questionMark
  , (,) "screen"  fullscreen
  , (,) "preview" preview
  ]


--------------------------------------------------------------------------------

bold :: SvgStyle -> S.Svg
bold svgStyle =
    applyStyle svgStyle $ 
      S.path 
        ! d dirs
  where
    s = (strokeSize svgStyle) / 2
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
      l                         (k6 + 0.1)  0
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
      

--------------------------------------------------------------------------------

italic :: S.Svg
italic =
    S.path
      ! fill "none"
      ! (strokeWidth .: 2*w)
      ! strokeLinecap "round"
      ! d dirs
  where
    w  = 0.07
    kx = 0.2
    ky = 0.2
    ε  = 0.15
    dirs = mkPath $ do
      m   (1 - kx)        ky
      l   (1 - kx - 2*ε)  ky
      m   kx              (1 - ky)
      l   (kx + 2*ε)      (1 - ky)
      m   (kx + ε)        (1 - ky)
      l   (1 - kx - ε)    ky



--------------------------------------------------------------------------------

link :: S.Svg
link =
    g ! A.transform (S.rotateAround 45 0.5 0.5)
      $ do
        topPart
        topPart ! A.transform (rotateAround 180 0 0)
  where
    topPart = S.path ! d topPath ! stroke "none"
    -----------------
    w1 = 0.2
    w2 = 0.09
    -----------------
    h1 = 0.5 - w2
    h2 = 0.2
    h3 = 0.15
    h4 = h3 - (w1 - w2)
    topPath = mkPath $ do
      m   (0.5 - w1)  (0.5 - h3)
      l   (0.5 - w1)  (0.5 - h1)
      aa  w1 w1 0 True True (0.5 + w1) (0.5 - h1)
      l   (0.5 + w1)  (0.5 - h4)
      aa  w1 w1 0 False True 0.5 (0.5 + h3)
      aa  ((h3 - h4)/2)   ((h3 - h4)/2) 0 False True 0.5 (0.5 + h4)
      aa  w2 w2 0 False False (0.5 + w2) (0.5 - h4)
      l   (0.5 + w2)  (0.5 - h1)
      aa  w2 w2 0 True False (0.5 - w2) (0.5 - h1)
      l   (0.5 - w2) (0.5 - h2)
      S.z

--------------------------------------------------------------------------------

imageIcon :: S.Svg
imageIcon =
    do
      imageFrame
      mountain
      sun
  where
    sun =
      circle
        ! (cx .: 0.75)
        ! (cy .: 0.25)
        ! (r  .: 0.12)
        ! stroke "none"
    -------------------------
    x = 0.06
    imageFrame =
      S.path
        ! d framePath
        ! stroke "none"
    framePath = mkPath $ do
      m 0 0
      l 1 0
      l 1 1
      l 0 1
      l 0 0
      m x     x
      l x     (1-x)
      l (1-x) (1-x)
      l (1-x) x
      l x     x
      S.z
    -------------------------
    mountain =
      S.path
        ! A.d mountainPath
        ! stroke "none"
    mountainPath = mkPath $ do
      m   0      1
      l   0.35   0.35
      l   0.5    0.65
      l   0.7    0.5
      l   1      1
      S.z

--------------------------------------------------------------------------------

video :: S.Svg
video =
    do
      topLeftCorner
      botRightCorner
      otherTwoCorners
      triangle
  where
    otherTwoCorners =
      g (topLeftCorner >> botRightCorner)
        ! A.transform horizontalMirrorMatrix
    botRightCorner = 
      topLeftCorner
        ! A.transform (rotateAround 180 0 0)
    ---------------------------------
    topLeftCorner =
      S.path
        ! d boxPath
        ! stroke "none"
    y1 = 0.2
    y2 = 1 - y1
    w  = 1.618 * (y2 - y1)
    x1 = 0.5 - w/2
    boxPath = mkPath $ do
      m x1 0.5
      c (x1)
        (y1 + 0.15)
        (x1 )
        (y1 + 0.02)
        (x1 + 0.11)
        (y1)
      s (x1 + 0.20)
        (y1 - 0.01)
        (0.5)
        (y1 - 0.02)
      l 0.51 0.51
      S.z
    ---------------------------------
    triangle =
      polygon
        ! fill "#ffffff"
        ! points ps
        ! stroke "none"
    t = 0.41
    h = 0.26
    t1 = (,)  t      (0.5 - h/2)
    t2 = (,)  t      (0.5 + h/2)
    t3 = (,) (t + h) (0.5)
    ps =
      toValue
      $ intercalate " "
      $ map (\ (p1,p2) -> show p1 ++ "," ++ show p2)
        [t1, t2, t3]

--------------------------------------------------------------------------------

horizontalBars :: S.Svg
horizontalBars =
    do
      S.path ! d topLine ! stroke "none"
      S.path ! d midLine ! stroke "none"
      S.path ! d botLine ! stroke "none"
  where
    w  = 0.12
    x1 = 0.35
    x2 = 1
    y1 = 0.2
    y2 = 0.5
    y3 = 0.8
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


bulletList :: S.Svg
bulletList =
    do
      horizontalBars
      bullets 
  where
    radius = 0.09
    x1 = 0.1
    y1 = 0.2
    y2 = 0.5
    y3 = 0.8
    bullets = do
      circle ! (cx .: x1) ! (cy .: y1) ! (r .: radius) ! stroke "none"
      circle ! (cx .: x1) ! (cy .: y2) ! (r .: radius) ! stroke "none"
      circle ! (cx .: x1) ! (cy .: y3) ! (r .: radius) ! stroke "none"


numberList :: Svg
numberList =
    do
      horizontalBars
      numbers
  where
    f x = x ! dominantBaseline "alphabetical"
            ! textAnchor "middle"
            ! fontWeight "bold"
            ! fontSize   "0.3"
    x1 = 0.05
    y1 = 0.28
    y2 = 0.58
    y3 = 0.88
    numbers = do
      f $ S.text_ "1" ! (A.x .: x1) ! (A.y .: y1) ! stroke "none"
      f $ S.text_ "2" ! (A.x .: x1) ! (A.y .: y2) ! stroke "none"
      f $ S.text_ "3" ! (A.x .: x1) ! (A.y .: y3) ! stroke "none"

--------------------------------------------------------------------------------

header :: S.Svg
header =
    do
      S.path ! d line1 ! stroke "none"
      S.path ! d line2 ! stroke "none"
      S.path ! d line3 ! stroke "none" ! opacity "0.4"
      S.path ! d line4 ! stroke "none" ! opacity "0.4"
      S.path ! d line5 ! stroke "none" ! opacity "0.4"
  where
    l1 = 0.05
    l2 = 0.25
    r2 = 0.75
    r1 = 0.95
    h = 1/6
    w = 0.1
    line1 = mkPath $ do
       m l1 (1*h - w/2)
       l r1 (1*h - w/2)
       l r1 (1*h + w/2)
       l l1 (1*h + w/2)
       S.z
    line2 = mkPath $ do
       m l2 (2*h - w/2)
       l r2 (2*h - w/2)
       l r2 (2*h + w/2)
       l l2 (2*h + w/2)
       S.z
    line3 = mkPath $ do
       m l1 (3*h - w/2)
       l r1 (3*h - w/2)
       l r1 (3*h + w/2)
       l l1 (3*h + w/2)
       S.z
    line4 = mkPath $ do
       m l2 (4*h - w/2)
       l r2 (4*h - w/2)
       l r2 (4*h + w/2)
       l l2 (4*h + w/2)
       S.z
    line5 = mkPath $ do
       m l1 (5*h - w/2)
       l r1 (5*h - w/2)
       l r1 (5*h + w/2)
       l l1 (5*h + w/2)
       S.z


horizontalRule :: S.Svg
horizontalRule =
    do
      S.path ! stroke "none" ! d line1 ! opacity "0.4"
      S.path ! stroke "none" ! d line2 ! opacity "0.4"
      line3  ! stroke "none"
      S.path ! stroke "none" ! d line4 ! opacity "0.4"
      S.path ! stroke "none" ! d line5 ! opacity "0.4"
  where
    l1 = 0.05
    l2 = 0.25
    r2 = 0.75
    r1 = 0.95
    h = 1/6
    w = 0.1
    line1 = mkPath $ do
       m l1 (1*h - w/2)
       l r1 (1*h - w/2)
       l r1 (1*h + w/2)
       l l1 (1*h + w/2)
       S.z
    line2 = mkPath $ do
       m l2 (2*h - w/2)
       l r2 (2*h - w/2)
       l r2 (2*h + w/2)
       l l2 (2*h + w/2)
       S.z
    square leftX =
      S.rect
        ! (A.x    .: leftX)
        ! (A.y    .: 3*h - w/2)
        ! (width  .: w)
        ! (height .: w)
    line3 = do
      square l1
      square l2
      square (0.5 - w/2)
      square (r2 - w)
      square (r1 - w)
    line4 = mkPath $ do
       m l2 (4*h - w/2)
       l r2 (4*h - w/2)
       l r2 (4*h + w/2)
       l l2 (4*h + w/2)
       S.z
    line5 = mkPath $ do
       m l1 (5*h - w/2)
       l r1 (5*h - w/2)
       l r1 (5*h + w/2)
       l l1 (5*h + w/2)
       S.z


--------------------------------------------------------------------------------

undo :: S.Svg
undo =
  curvyArrowLeft


redo :: S.Svg
redo =
  curvyArrowRight


curvyArrowLeft :: Svg
curvyArrowLeft =
  S.path 
    ! d curvyArrowLeftPath
    ! stroke "none"


curvyArrowRight :: Svg
curvyArrowRight =
  S.path 
    ! d curvyArrowLeftPath
    ! stroke "none"
    ! A.transform horizontalMirrorMatrix


curvyArrowLeftPath :: AttributeValue
curvyArrowLeftPath =
    mkPath $ do
      m   a1  a2
      aa  r1  r1  0 False False b1 b2
      l   d1  d2
      l   f1  f2
      l   e1  e2
      l   c1  c2
      aa  r2  r2  0 False True a1 a2
  where
    k1 = 0.45
    r1 = 0.43
    r2 = 0.305
    (a1,a2) = ( 0.55 , 1    )
    (b1,b2) = ( k1   , 0.15 )
    (c1,c2) = ( k1   , 0.40 )
    (d1,d2) = ( k1   , 0.00 )
    (e1,e2) = ( k1   , 0.55 )
    (f1,f2) = ( 0.15 , (b2+c2)/2)


--------------------------------------------------------------------------------

questionMark :: S.Svg
questionMark =
    do
      topArm
      dot
  where
    (a1,a2) = ( 0.40 ,  0.40 )
    (b1,b2) = ( 0.30 ,  0.45 )
    (c1,c2) = ( 0.25 ,  0.40 )
    (d1,d2) = ( 0.25 ,  0.25 )
    (e1,e2) = ( 0.25 ,  0.15 )
    (f1,f2) = ( 0.25 ,  0.10 )
    (g1,g2) = ( 0.50 ,  0.10 )
    (h1,h2) = ( 0.60 ,  0.10 )
    (i1,i2) = ( 0.80 ,  0.10 )
    (j1,j2) = ( 0.80 ,  0.25 )
    (k1,k2) = ( 0.80 ,  0.45 )
    (l1,l2) = ( 0.40 ,  0.50 )
    (m1,m2) = ( 0.50 ,  0.68 )
    topArm =
      S.path
        ! d topArmPath
        ! (strokeWidth .: 0.13)
        ! fill "none"
        ! strokeLinecap "round"
    topArmPath = S.mkPath $ do
      m a1 a2
      c b1 b2 c1 c2 d1 d2
      c e1 e2 f1 f2 g1 g2
      c h1 h2 i1 i2 j1 j2
      c k1 k2 l1 l2 m1 m2
    dot = S.circle
      ! (cx .: 0.5)
      ! (cy .: 0.9)
      ! (r  .: 0.08)
      ! stroke "none"

--------------------------------------------------------------------------------


fullscreen :: S.Svg
fullscreen =
    do
      topLeftCorner
      topLeftCorner ! A.transform (rotateAround  90 0.5 0.5)
      topLeftCorner ! A.transform (rotateAround 180 0.5 0.5)
      topLeftCorner ! A.transform (rotateAround 270 0.5 0.5)
 where
   w = 0.05
   k1 = 0.15
   k2 = 0.35
   topLeftCorner =
     S.path
      ! d dirsTopLeft
      ! (strokeWidth .: 2*w)
      ! fill "none"
   dirsTopLeft = mkPath $ do
     m   k1  k2
     l   k1  k1 
     l   k2  k1


--------------------------------------------------------------------------------

preview :: S.Svg
preview = 
    do
      lines
      rectangle
  where
    w = 0.05
    kx = 0.1
    ky = 0.2
    lines =
      S.path
        ! fill "none"
        ! (strokeWidth .: 2*w)
        ! strokeLinecap "round"
        ! d linesDirs
    rectangle =
      S.path 
        ! (strokeWidth .: 2*w)
        ! strokeLinejoin "round"
        ! d rectDirs
    linesDirs = mkPath $ do
      m   kx          ky
      l   (0.5 - kx)  ky
      m   kx          (2*ky)
      l   (0.5 - kx)  (2*ky)
      m   kx          (3*ky)
      l   (0.5 - kx)  (3*ky)
      m   kx          (4*ky)
      l   (0.5 - kx)  (4*ky)
    rectDirs = mkPath $ do
      m   (0.5 + kx)  ky
      l   (1   - kx)  ky
      l   (1   - kx)  (4*ky)
      l   (0.5 + kx)  (4*ky)
      S.z
    


--------------------------------------------------------------------------------