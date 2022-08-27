{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Tools where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgTools :: [ (String , S.Svg) ]
svgTools =
  [ (,) "lock"       lock
  , (,) "key"        key
  , (,) "keyWithArc" keyWithArc
  , (,) "cog6"       cog6
  , (,) "cog9"       cog9
  ]


--------------------------------------------------------------------------------


lock :: S.Svg
lock =
  S.g $ do
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


key :: S.Svg
key =
  S.path
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


keyWithArc :: S.Svg
keyWithArc =
    S.g $ do
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
    

cogwheel :: Int -> Float -> S.Svg
cogwheel n eps =
    S.path
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


cog6 :: S.Svg
cog6 = cogwheel 6 0.18


cog9 :: S.Svg
cog9 = cogwheel 9 0.12