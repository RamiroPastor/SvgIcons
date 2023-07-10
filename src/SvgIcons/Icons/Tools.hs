{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Tools 
  ( svgTools
  , cogwheel
  , cog6
  , cog9
  , key
  , keyWithArc
  , lock
  , key1
  , bell
  , wrench
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils



{- |
A list with all the icons of this module, 
together with appropriate names.

>svgTools :: [ (String , S.Svg) ]
>svgTools =
>  [ (,) "cog6"       cog6
>  , (,) "cog9"       cog9
>  , (,) "key"        key
>  , (,) "keyWithArc" keyWithArc
>  , (,) "lock"       lock
>  , (,) "key1"       key1
>  , (,) "bell"       bell
>  , (,) "wrench"     wrench
>  ]
-}
svgTools :: [ (String , S.Svg) ]
svgTools =
  [ (,) "cog6"       cog6
  , (,) "cog9"       cog9
  , (,) "key"        key
  , (,) "keyWithArc" keyWithArc
  , (,) "lock"       lock
  , (,) "key1"       key1
  , (,) "bell"       bell
  , (,) "wrench"     wrench
  ]


--------------------------------------------------------------------------------



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/lock_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/lock_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/lock_strk.svg)
-}
lock :: S.Svg
lock =
  S.g 
    ! A.class_ "HaskellSvgIcons__lock"
    $ do
      S.path 
        ! A.d arm
      S.path
        ! fillRule "evenodd"
        ! A.d body
  where
    aw  =  0.07
    ax  =  0.4
    ay1 = -0.1
    ay2 = -0.48
    arm =
      mkPath $ do
        m   (-ax - aw)  ay1
        l   (-ax - aw)  ay2
        aa  ( ax + aw) (ax + aw)  0  True  True  ( ax + aw) ay2
        l   ( ax + aw)  ay1
        l   ( ax - aw)  ay1
        l   ( ax - aw)  ay2
        aa  ( ax - aw) (ax - aw)  0  True  False (-ax + aw) ay2
        l   (-ax + aw)  ay1
        S.z
    ----------------------------------------
    bx  = 0.7
    by1 = ay1
    by2 = 0.95
    kr  = 0.14
    kw  = 0.076
    ky1 = 0.4
    ky2 = 0.68
    body = mkPath $ do
      m  (-bx) by1
      l  (-bx) by2
      l  ( bx) by2
      l  ( bx) by1
      S.z
      m  (-kw) ky1
      l  (-kw) ky2
      aa   kw  kw  0  True  False ( kw) ky2
      l  ( kw) ky1
      aa   kr  kr  0  True  False (-kw) ky1
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/key_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/key_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/key_strk.svg)
-}
key :: S.Svg
key =
  S.path
    ! A.class_ "HaskellSvgIcons__key"
    ! fillRule "evenodd"
    ! A.d keyPath
  where
    w  = 0.1
    x0 = 0.3
    x1 = 0
    x2 = 0.5
    x3 = 0.8
    y1 = 0.3
    r1 = 0.25
    keyPath = mkPath $ do
      m   (x1-2*w) (-0.005)
      aa   r1       r1      0  True  False (x1-2*w) 0
      S.z
      m    x1      (-w)
      aa  (r1+2*w) (r1+2*w) 0  True  False  x1      w
      l   (x2 - w) ( w)
      l   (x2 - w) (y1)
      aa  ( w)     ( w)     0  True  False (x2 + w) y1
      l   (x2 + w) ( w)
      l   (x3 - w) ( w)
      l   (x3 - w) (y1)
      aa  ( w)     ( w)     0  True  False (x3 + w) y1
      l   (x3 + w) (-w)
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/keyWithArc_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/keyWithArc_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/keyWithArc_strk.svg)
-}
keyWithArc :: S.Svg
keyWithArc =
    S.g 
      ! A.class_ "HaskellSvgIcons__keyWithArc"
      $ do
        key ! A.transform (translate (-0.4) 0 <> S.scale 0.6 0.6)
        arc ! A.transform (S.scale 0.6 0.6)
  where
    w  = 0.1
    r1 = 1.3
    r2 = r1 + 2*w
    π  = pi
    α  = π / 4
    x1 = r1 * cos α
    y1 = r1 * sin α
    x2 = r2 * cos α
    y2 = r2 * sin α
    arc =
      S.path
        ! A.d arcPath
    arcPath = mkPath $ do
      m   (-x1) (-y1)
      aa    w     w    0  True  True  (-x2) (-y2)
      aa    r2    r2   0  True  True  (-x2) ( y2)
      aa    w     w    0  True  True  (-x1) ( y1)
      aa    r1    r1   0  True  False (-x1) (-y1)
      S.z
    


{- |
Takes a natural number @n@ which is the number of cogs, 
and a real number @eps@ which controls how 'pointy' the cogs are. 
-}
cogwheel :: Int -> Float -> S.Svg
cogwheel n eps =
    S.path
      ! A.class_ "HaskellSvgIcons__cogwheel"
      ! A.d cogPath
  where
    r1 = 0.4  :: Float
    r2 = 0.66 :: Float
    r3 = 0.94 :: Float
    a  = (2 * pi) / (2 * fromIntegral n)
    makeAngles k'  =
      let k = fromIntegral k'
      in [ k*a - eps, k*a + eps ]
    makePoint r α = ( r * cos α , r * sin α)
    outer = map (makePoint r3) $ concatMap makeAngles $ filter even [0 .. 2*n]
    inner = map (makePoint r2) $ concatMap makeAngles $ filter odd  [0 .. 2*n]
    f ((a1,a2):(b1,b2):outs) ((c1,c2):(d1,d2):ins) = do
      l  a1 a2
      l  b1 b2
      l  c1 c2
      aa r2 r2 0 False True d1 d2
      f outs ins
    f _ _ = S.z
    cogPath = mkPath $ do
      m ( r1)  0
      aa r1 r1 0 True False (-r1) 0
      aa r1 r1 0 True False ( r1) 0
      m (fst $ head outer) (snd $ head outer)
      f outer inner



{- |
prop> cog6 = cogwheel 6 0.18

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/cog6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/cog6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/cog6_strk.svg)
-}
cog6 :: S.Svg
cog6 = cogwheel 6 0.18



{- |
prop> cog = cogwheel 9 0.12

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/cog9_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/cog9_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/cog9_strk.svg)
-}
cog9 :: S.Svg
cog9 = cogwheel 9 0.12



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/key1_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/key1_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/key1_strk.svg)
-}
key1 :: S.Svg
key1 =
    S.g 
      ! A.class_ "HaskellSvgIcons__key1"
      $ S.path
          ! A.d keyPath
          ! A.transform (rotateAround 45 0 0)
  where
    x1 = -0.1
    y1 =  0.2
    r1 =  0.51
    x2 =  1.05
    x3 =  x2 + y1
    x0 = -0.6
    r0 =  0.15
    keyPath = mkPath $ do
      m   x1   y1
      aa  r1   r1   0   True   True    x1  (-y1)
      l   x2 (-y1)
      l   x3    0
      l   x2   y1
      lr (-0.1)    0
      lr (-0.1)  (-0.1)
      lr (-0.1)  ( 0.1)
      lr (-0.1)  (-0.1)
      lr (-0.1)  ( 0.1)
      lr (-0.1)  (-0.1)
      lr (-0.1)  ( 0.1)
      lr (-0.1)  (-0.1)
      lr (-0.15) ( 0.15)
      lr (-0.05) (-0.05)
      S.z
      m   x0    0
      aa  r0   r0   0   True   False   x0   0.001
      S.z
    
  

{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/bell_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/bell_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/bell_strk.svg)
-}
bell :: S.Svg
bell =
    S.path
      ! A.class_ "HaskellSvgIcons__bell"
      ! A.d dirs
  where
    r1 =  0.5
    x1 =  r1 + 0.25
    y0 = -0.3
    y1 =  0.5
    y01 = (y1 + y0) / 2
    x2 =  0.1
    r2 =  0.17
    dirs = mkPath $ do
      m    x1   y1
      c    x1   y01   r1   y01   r1   y0
      aa   r1   r1    0   True   False  (-r1)  y0
      c  (-r1)  y01 (-x1)  y01 (-x1)  y1
      S.z
      m  (-x2)  y1
      aa   r2   r2    0   True   False    x2   y1
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/wrench_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/wrench_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/tools/wrench_strk.svg)
-}
wrench :: S.Svg
wrench =
    S.path
      ! A.class_ "HaskellSvgIcons__wrench"
      ! A.d dirs
      ! A.strokeLinejoin "round"
  where
    x0 =  0
    x1 =  0.27
    x2 =  0.2
    x3 = (x1 + x2)/2
    y0 = -0.35
    y1 = -0.6
    y2 = -0.95
    y3 =  0
    y4 =  0.7
    r1 =  0.52
    r2 =  x3
    r3 =  0.1
    dirs = mkPath $ do
      m   x0  y0
      l (-x1) y1
      l (-x1) y2
      aa  r1  r1  0  False False (-x2) y3
      l (-x3) y4
      aa  r2  r2  0  True  False   x3  y4
      l   x2  y3
      aa  r1  r1  0  False False   x1  y2
      l   x1  y1
      S.z
      m  (x0 - r3) y4
      aa  r3  r3  0  True  True   (x0 - r3) (y4 + 0.0001)
      S.z
    