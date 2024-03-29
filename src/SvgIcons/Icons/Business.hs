{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Icons.Business 
  ( svgBusiness
  , analytics
  , bullseye
  , company
  , connections
  , creditCard
  , creditIn
  , creditOut
  , coinColumn
  , coinPile
  , dividends
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils
import SvgIcons.Icons.Arrows (bigArrowLeft, bigArrowRight)



{- |
A list with all the icons of this module, 
together with appropriate names.

>svgBusiness :: [ (String , S.Svg) ]
>svgBusiness =
>  [ (,) "analytics"   analytics
>  , (,) "bullseye"    bullseye
>  , (,) "company"     company
>  , (,) "connections" connections
>  , (,) "creditCard"  creditCard
>  , (,) "creditIn"    creditIn
>  , (,) "creditOut"   creditOut
>  , (,) "coinPile"    coinPile
>  , (,) "dividends"  (dividends "$")
>  ]
-}
svgBusiness :: [ (String , S.Svg) ]
svgBusiness =
  [ (,) "analytics"   analytics
  , (,) "bullseye"    bullseye
  , (,) "company"     company
  , (,) "connections" connections
  , (,) "creditCard"  creditCard
  , (,) "creditIn"    creditIn
  , (,) "creditOut"   creditOut
  , (,) "coinPile"    coinPile
  , (,) "dividends"  (dividends "$")
  ]


--------------------------------------------------------------------------------

{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/company_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/company_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/company_strk.svg)
-}
company :: S.Svg
company =
  S.g 
    ! A.class_ "HaskellSvgIcons__company"
    $ do
      S.path
        ! d leftBuildingPath
      S.path
        ! d leftWindowsPath
        ! strokeDasharray "0.12 0.06"
      S.path
        ! d rightBuildingPath
      S.path
        ! d rightWindowsPath
        ! strokeDasharray "0.05"
  where
    x1 = -0.92
    x2 = -0.72
    x3 =  0
    x4 =  0.22
    x5 =  0.92
    y1 = -0.9
    y2 = -0.75
    y3 = (y1 + y4) / 2
    y4 = -0.3
    y5 = -0.2
    y6 =  0.8
    y7 =  0.9
    k1 = (x3 - x2) / 3
    k2 = (x5 - x4) / 4
    doorH = 0.24
    ----------------------------------------
    leftBuildingPath =
      mkPath $ do
        m   x1  y7
        l   x1  y2
        l   x2  y2
        l   x2  y1
        l   x3  y1
        l   x3  y2
        l   x4  y2
        l   x4  y7
        doorPath
        S.z
    rightBuildingPath =
      mkPath $ do
        m   x4  y4
        l   x5  y4
        l   x5  y7
        l   x4  y7
        S.z
    doorPath = 
      do
        l (x2 + 2*k1)  y7
        l (x2 + 2*k1) (y7 - doorH)
        l (x2 +   k1) (y7 - doorH)
        l (x2 +   k1)  y7
    leftWindowsPath =
      mkPath $ do
        m   (x2 + 0*k1)  y3
        l   (x2 + 0*k1)  y6
        m   (x2 + 1*k1)  y3
        l   (x2 + 1*k1)  (y7 - 1.5 * doorH)
        m   (x2 + 2*k1)  y3
        l   (x2 + 2*k1)  (y7 - 1.5 * doorH)
        m   (x2 + 3*k1)  y3
        l   (x2 + 3*k1)  y6
    rightWindowsPath =
      mkPath $ do
        m   (x4 + 1*k2)  y5
        l   (x4 + 1*k2)  y6
        m   (x4 + 2*k2)  y5
        l   (x4 + 2*k2)  y6
        m   (x4 + 3*k2)  y5
        l   (x4 + 3*k2)  y6



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/connections_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/connections_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/connections_strk.svg)
-}
connections :: Svg
connections = 
    S.g 
      ! A.class_ "HaskellSvgIcons__connections"
      $ do
        circ x0 y0 r0
        circ x1 y1 r1
        circ x2 y2 r2
        circ x3 y3 r3
        circ x4 y4 r4
        circ x5 y5 r5
        circ x6 y6 r6
        circ x7 y7 r7
        circ x8 y8 r8
        circ x9 y9 r9
        S.path
          ! A.fill "none"
          ! d (mkPath connectingLines)
  where
    rad1 = 0.2
    rad2 = 0.12
    (x0,y0,r0) = (,,)  ( 0   )  ( 0   )  0.3
    (x1,y1,r1) = (,,)  (-0.64)  ( 0   )  rad1
    (x2,y2,r2) = (,,)  ( 0.56)  (-0.4 )  rad1
    (x3,y3,r3) = (,,)  ( 0.56)  ( 0.4 )  rad1
    (x4,y4,r4) = (,,)  (-0.64)  (-0.6 )  rad2
    (x5,y5,r5) = (,,)  (-0.82)  ( 0.4 )  rad2
    (x6,y6,r6) = (,,)  (-0.4 )  ( 0.7 )  rad2
    (x7,y7,r7) = (,,)  ( 0.1 )  (-0.74)  rad2
    (x8,y8,r8) = (,,)  ( 0.80)  (-0.8 )  rad2
    (x9,y9,r9) = (,,)  ( 0.56)  ( 0.8 )  rad2
    circ c1 c2 radius =
      circle
        ! (cx .: c1) 
        ! (cy .: c2) 
        ! (r .: radius)
    connect (p1,p2,radius1) (q1,q2,radius2) =
      let
        d = distance (p1,p2) (q1,q2)
        k1 = radius1 / d
        k2 = radius2 / d
      in do
        m  (k2*p1 + q1 - k2*q1)  (k2*p2 + q2 - k2*q2)
        l  (p1 - k1*p1 + k1*q1)  (p2 - k1*p2 + k1*q2)
    connectingLines = do
      connect (x0,y0,r0) (x1,y1,r1)
      connect (x0,y0,r0) (x2,y2,r2)
      connect (x0,y0,r0) (x3,y3,r3)
      connect (x1,y1,r1) (x4,y4,r4)
      connect (x1,y1,r1) (x5,y5,r5)
      connect (x1,y1,r1) (x6,y6,r6)
      connect (x2,y2,r2) (x7,y7,r7)
      connect (x2,y2,r2) (x8,y8,r8)
      connect (x3,y3,r3) (x9,y9,r9)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/analytics_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/analytics_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/analytics_strk.svg)
-}
analytics :: Svg
analytics = 
    S.g 
      ! A.class_ "HaskellSvgIcons__analytics"
      $ do
        S.path
          ! A.fill "none"
          ! A.d axesPath
        S.path ! A.d (bar x1 y1)
        S.path ! A.d (bar x2 y2)
        S.path ! A.d (bar x3 y3)
  where
    ax =  0.96
    ay =  0.96
    w  =  0.14
    x1 = -0.5
    x2 =  0
    x3 =  0.5
    y1 = -0.1
    y2 = -0.4
    y3 = -0.7
    axesPath = mkPath $ do
      m  (-ax)  (-ay)
      l  (-ax)  ( ay)
      l  ( ax)  ( ay)
    bar px py = mkPath $ do
      m  (px - w)  ay
      l  (px - w)  py
      l  (px + w)  py
      l  (px + w)  ay
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/bullseye_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/bullseye_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/bullseye_strk.svg)
-}
bullseye :: Svg
bullseye = 
    S.g 
      ! A.class_ "HaskellSvgIcons__bullseye"
      $ do
        S.path
          ! A.strokeLinecap "round"
          ! A.d circles
        S.path
          ! strokeLinecap "round"
          ! fill "none"
          ! A.d (mkPath $ stick >> feathers)
  where
    distanceToCenter x y = distance (x,y) (0,0)
    (p1,k1) = (,) (-0.6 )  0.1
    (p2,k2) = (,) (-0.44)  0.07
    (p3,k3) = (,) (-0.28)  0.07
    (p4,k4) = (,) (-0.12)  0.05
    d1 = distanceToCenter (p1 + k1) (p1 - k1)
    d2 = distanceToCenter (p2 + k2) (p2 - k2)
    d3 = distanceToCenter (p3 + k3) (p3 - k3)
    d4 = distanceToCenter (p4 + k4) (p4 - k4)
    circles = mkPath $ do
      m                         (p1 + k1) (p1 - k1)
      aa  d1 d1  0  True  True  (p1 - k1) (p1 + k1)
      m                         (p2 + k2) (p2 - k2)
      aa  d2 d2  0  True  True  (p2 - k2) (p2 + k2)
      m                         (p3 + k3) (p3 - k3)
      aa  d3 d3  0  True  True  (p3 - k3) (p3 + k3)
      m                         (p4 + k4) (p4 - k4)
      aa  d4 d4  0  True  True  (p4 - k4) (p4 + k4)
    fl = 0.2   -- feather length
    q1 = -0.76
    q2 = -0.68
    q3 = -0.6
    stick = do
      m    q1      q1
      l  (-0.01) (-0.01)
    feathers = do
      m   q1         q1
      l   q1         (q1 - fl)
      m   q1         q1
      l   (q1 - fl)  q1
      m   q2         q2
      l   q2         (q2 - fl)
      m   q2         q2
      l   (q2 - fl)  q2
      m   q3         q3
      l   q3         (q3 - fl)
      m   q3         q3
      l   (q3 - fl)  q3



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditCard_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditCard_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditCard_strk.svg)
-}
creditCard :: Svg
creditCard =
    S.g 
      ! A.class_ "HaskellSvgIcons__creditCard"
      $ do
        S.path
          ! A.fill "none"
          ! A.d cardPath
        S.path
          ! A.d cardBand
        S.path
          ! A.fill "none"
          ! A.strokeLinejoin "round"
          ! A.d chip
  where
    x0 = 0.93
    y0 = x0 * 0.618
    r0 = 0.1
    y1 = -0.4
    y2 = -0.15
    cw = 1.618 * ch
    ch = 0.3
    cx = -0.7
    cy = 0.5 * (y0 + y2) - 0.5 * ch
    cardPath = mkPath $ do
      m    0          y0
      l  ( x0 - r0)   y0
      aa   r0   r0    0   False  False    x0        ( y0 - r0)
      l    x0       (-y0 + r0)
      aa   r0   r0    0   False  False  ( x0 - r0)  (-y0)
      l  (-x0 + r0) (-y0)
      aa   r0   r0    0   False  False  (-x0)       (-y0 + r0)
      l  (-x0)      ( y0 - r0)
      aa   r0   r0    0   False  False  (-x0 + r0)    y0
      S.z
    cardBand = mkPath $ do
      m   (-x0)  y1
      l   ( x0)  y1
      l   ( x0)  y2
      l   (-x0)  y2
      S.z
    chip = mkPath $ do
      m     cx      cy
      lr    cw      0
      lr    0       ch
      lr  (-cw)     0
      S.z
      ---
      mr  ( cw/3)   0
      lr    0       ch
      mr    0     (-ch/3)
      lr  (-cw/3)   0
      mr    0     (-ch/3)
      lr  ( cw/3)   0
      ---
      mr  ( cw/3) (-ch/3)
      lr    0       ch
      mr    0     (-ch/3)
      lr  ( cw/3)   0
      mr    0     (-ch/3)
      lr  (-cw/3)   0
    
  

{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditIn_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditIn_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditIn_strk.svg)
-}
creditIn :: Svg
creditIn =
  S.g 
    ! A.class_ "HaskellSvgIcons__creditIn"
    $ do
      creditCard
      bigArrowLeft 
        ! A.transform (translate 0.45 0.22 <> S.scale 0.25 0.25)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditOut_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditOut_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/creditOut_strk.svg)
-}
creditOut :: Svg
creditOut =
  S.g 
    ! A.class_ "HaskellSvgIcons__creditOut"
    $ do
      creditCard
      bigArrowRight 
        ! A.transform (translate 0.45 0.22 <> S.scale 0.25 0.25)


{- |
Helper for `coinPile` icon.

Draws a column of (n+1) coins.
-}
coinColumn 
  :: Int    -- ^ n
  -> Float  -- ^ coordinate x of the center of the column
  -> Float  -- ^ coordinate y of the lowest coin
  -> Svg    -- ^ resulting svg
coinColumn n0 x0 y0 =
    S.g $ do
      S.path
        ! A.d (coinColumnDirs n x0 y0)
      S.ellipse
        ! (A.cx .: x0)
        ! (A.cy .: y0 - (n+1)*ch)
        ! (A.rx .: cw)
        ! (A.ry .: ry)
  where
    n = fromIntegral n0
    cw = 0.3  -- half width  of the coin 
    ch = 0.15 -- full height of the coin
    ry = 0.18
    coin x0 y0 = do
      m   (x0 - cw)  (y0 - ch)
      l   (x0 - cw)   y0
      aa   cw   ry  0  False False (x0 + cw) y0
      l   (x0 + cw)  (y0 - ch)
    coinColumnDirs n x0 y0 = mkPath $ do
      mapM_ (coin x0) $ map (\k -> y0 - k*ch) [0..n]
      aa   cw   ry  0  False True  (x0 - cw) (y0 - (n+1) * ch)
      aa   cw   ry  0  False True  (x0 + cw) (y0 - (n+1) * ch)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/coinPile_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/coinPile_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/coinPile_strk.svg)
-}
coinPile :: Svg
coinPile =
  S.g
    ! A.class_ "HaskellSvgIcons__coinPile" 
    $ do
      (coinColumn 7   0    0.5 )
      (coinColumn 5 (-0.6) 0.55)
      (coinColumn 3 ( 0.6) 0.55)
      (coinColumn 1 (-0.3) 0.75)
      (coinColumn 5 ( 0.3) 0.75)
    


{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/dividends_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/dividends_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/business/dividends_strk.svg)

Takes a string which should be the money symbol you want to use (or an empty string).
-}
dividends :: String -> Svg
dividends moneySymbol =
    S.g
      ! A.class_ "HaskellSvgIcons__dividends"
      $ do
        S.path
          ! A.d cakePath
          ! A.fill "none"
          ! A.strokeLinejoin "round"
        S.text_ "%"
          ! (A.x .: k2 + 0.5*r1)
          ! (A.y .: k2 + 0.5*r1)
          ! A.stroke "none"
          ! A.dominantBaseline "middle"
          ! A.textAnchor "middle"
          ! A.fontFamily "Arial Black"
          ! A.fontSize "0.35"
        S.text_ (S.toSvg moneySymbol)
          ! (A.x .: sx)
          ! (A.y .: sy)
          ! A.stroke "none"
          ! A.dominantBaseline "middle"
          ! A.textAnchor "middle"
          ! A.fontFamily "Arial Black"
          ! A.fontSize "0.75"
  where
    r1 =  0.78
    k1 =  0.12
    k2 =  0.06
    (sx, sy) = (-0.5 , -0.1)
    cakePath = mkPath $ do
      m  (-k1)      (-k1)
      l  (-k1 + r1) (-k1)
      aa   r1   r1    0    True  False  (-k1)  (-k1 + r1)
      S.z
      m    k1         k1
      l  ( k1 + r1) ( k1)
      aa   r1   r1    0    False True   ( k1)  ( k1 + r1)
      S.z
      