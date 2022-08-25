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