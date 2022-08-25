{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Human where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgHuman :: [ (String , S.Svg) ]
svgHuman =
  [ (,) "eyeOpened"  eyeOpened
  , (,) "eyeStriked" eyeStriked
  , (,) "person"     person
  , (,) "people"     people
  , (,) "carnet"     carnet
  ]


--------------------------------------------------------------------------------

eyeOpened :: S.Svg
eyeOpened =
  S.g $ do
    eye
    pupil
    glow
  where
    w  = 0.9
    c1 = 0
    c2 = -0.2
    cr = 0.46
    k  = 0.25
    eye = 
      S.path
        ! A.d eyePath
        ! A.strokeLinejoin "round"
        ! A.fill "white"
    eyePath = S.mkPath $ do
      m   (-w)   0
      c   (-0.5) (-0.9) ( 0.5) (-0.9) ( w) 0
      c   ( 0.5) ( 0.9) (-0.5) ( 0.9) (-w) 0
      S.z
    pupil =
      S.circle
        ! (A.cx .: c1)
        ! (A.cy .: c2)
        ! (A.r  .: cr)
    glow =
      S.path
        ! A.d glowPath
        ! A.fill "none"
        ! A.stroke "white"
        ! (A.strokeWidth .: 0.5 * k)
        ! A.strokeLinecap "round"
    glowPath = S.mkPath $ do
      m   (c1 - k)   c2
      aa  k  k  0  False  True  c1  (c2 - k)



eyeStriked :: S.Svg
eyeStriked =
  S.g $ do
    eyeOpened
    bar
  where
    k = 0.9
    bar =
      S.path
        ! A.d barPath
        ! A.strokeLinecap "round"
    barPath = S.mkPath $ do
      m  ( k) (-k)
      l  (-k) ( k)


person :: S.Svg
person =
  S.g $ do
    simpleShoulders
    simpleHead
  where
    kx =  0.7
    ky =  0.52
    kr = (1 - kx)
    simpleHead =
      circle
        ! cx "0"
        ! cy "-0.5"
        ! r  "0.35"
    simpleShoulders =
      S.path
        ! d shouldersPath
    shouldersPath =
      mkPath $ do
        m  kx  ky
        aa kr  kr   0 True False (-kx) ky
        aa kr  0.15 0 True False   kx  ky


people :: S.Svg
people =
  S.g $ do
    person ! A.transform (translate   0.4  (-0.2) <> S.scale 0.8 0.8)
    person ! A.transform (translate (-0.4) (-0.2) <> S.scale 0.8 0.8)
    person ! A.transform (translate   0    ( 0.2) <> S.scale 0.9 0.9)


carnet :: S.Svg
carnet =
  S.g $ do
    cardBorder
    textLines ! A.transform (translate ( 0.4) 0 <> S.scale 0.5 0.5)
    person    ! A.transform (translate (-0.5) 0 <> S.scale 0.5 0.5)
  where
    w1 = 0.01
    x1 = 1.618 * y1
    y1 = 0.58
    cardBorder =
      S.path
        ! d cardBorderPath
        ! fillRule "evenodd"
    cardBorderPath =
      mkPath $ do
        m   (-x1 - w1)  (-y1 - w1)
        l   ( x1 + w1)  (-y1 - w1)
        l   ( x1 + w1)  ( y1 + w1)
        l   (-x1 - w1)  ( y1 + w1)
        S.z
        m   (-x1 + w1)  (-y1 + w1)
        l   ( x1 - w1)  (-y1 + w1)
        l   ( x1 - w1)  ( y1 - w1)
        l   (-x1 + w1)  ( y1 - w1)
        S.z
    w2 =  0.06
    h1 = -0.5
    h2 =  0
    h3 =  0.5
    k1 = -0.7
    k2 =  0.7
    textLines =
      S.path
        ! d (mkPath $ line h1 >> line h2 >> line h3)
    line hy = do
      m   k1  (hy - w2)
      aa  w2  w2  0  True  False k1 (hy + w2)
      l   k2  (hy + w2)
      aa  w2  w2  0  True  False k2 (hy - w2)
      S.z
    