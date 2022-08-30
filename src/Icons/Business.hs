{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Business where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgBusiness :: [ (String , S.Svg) ]
svgBusiness =
  [ (,) "company"     company
  , (,) "connections" connections
  ]


--------------------------------------------------------------------------------


company :: S.Svg
company =
  S.g $ do
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


connections :: Svg
connections = 
    S.g $ do
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
    --------------------------------------------------
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