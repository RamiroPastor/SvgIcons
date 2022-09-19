{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Human 
  ( svgHuman
  , carnet
  , eyeOpened
  , eyeStriked
  , heartFat
  , heartSlim
  , people
  , person
  , talk
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Core.Utils


{- |
A list with all the icons of this module, 
together with appropriate names.

>svgHuman :: [ (String , S.Svg) ]
>svgHuman =
>  [ (,) "carnet"      carnet
>  , (,) "eyeOpened"   eyeOpened
>  , (,) "eyeStriked"  eyeStriked
>  , (,) "heartFat"    heartFat
>  , (,) "heartSlim"   heartSlim
>  , (,) "people"      people
>  , (,) "person"      person
>  , (,) "talk"        talk
>  ]
-}
svgHuman :: [ (String , S.Svg) ]
svgHuman =
  [ (,) "carnet"      carnet
  , (,) "eyeOpened"   eyeOpened
  , (,) "eyeStriked"  eyeStriked
  , (,) "heartFat"    heartFat
  , (,) "heartSlim"   heartSlim
  , (,) "people"      people
  , (,) "person"      person
  , (,) "talk"        talk
  ]


--------------------------------------------------------------------------------



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/eyeOpened_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/eyeOpened_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/eyeOpened_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/eyeStriked_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/eyeStriked_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/eyeStriked_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/person_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/person_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/person_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/people_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/people_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/people_strk.svg)
-}
people :: S.Svg
people =
  S.g $ do
    person ! A.transform (translate   0.4  (-0.2) <> S.scale 0.8 0.8)
    person ! A.transform (translate (-0.4) (-0.2) <> S.scale 0.8 0.8)
    person ! A.transform (translate   0    ( 0.2) <> S.scale 0.9 0.9)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/carnet_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/carnet_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/carnet_strk.svg)
-}
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/heartFat_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/heartFat_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/heartFat_strk.svg)
-}
heartFat :: Svg
heartFat =
    S.g $ do
      S.path
        ! A.d heartDirs
        ! A.strokeLinejoin "round"
        ! A.transform (translate 0 0.1 <> S.scale 1.2 1.2)
  where
    h = 0.06
    (h1x , h1y) = ( 0       , -0.6      )
    (h2x , h2y) = ( h1x - h ,  h1y - h  )
    (h3x , h3y) = ( h2y     ,  h2x      )
    (h4x , h4y) = ( 0       ,  0.6     )
    (hqx , hqy) = (-0.1     ,  0.6      )
    rh = 0.5 * distance (h2x,h2y) (h3x,h3y)
    heartDirs = mkPath $ do
      m   h1x h1y
      l   h2x h2y
      aa  rh  rh  0  False False  h3x  h3y
      q   hqx hqy h4x h4y
      q   (-hqx) ( hqy) (-h3x) ( h3y)
      aa  rh  rh  0  False False (-h2x) h2y
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/heartSlim_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/heartSlim_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/heartSlim_strk.svg)
-}
heartSlim :: Svg
heartSlim =
    S.g $ do
      S.path
        ! A.d heartDirs
        ! A.strokeLinejoin "round"
  where
    h = 0.2
    (h1x , h1y) = ( 0       , -0.6      )
    (h2x , h2y) = ( h1x - h ,  h1y - h  )
    (h3x , h3y) = ( h2y     ,  h2x      )
    (h4x , h4y) = ( 0       ,  0.9      )
    (hqx , hqy) = (-0.1     ,  0.4      )
    rh = 0.5 * distance (h2x,h2y) (h3x,h3y)
    heartDirs = mkPath $ do
      m   h1x h1y
      l   h2x h2y
      aa  rh  rh  0  False False  h3x  h3y
      q   hqx hqy h4x h4y
      q   (-hqx) ( hqy) (-h3x) ( h3y)
      aa  rh  rh  0  False False (-h2x) h2y
      S.z



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/talk_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/talk_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/human/talk_strk.svg)
-}
talk :: Svg
talk = 
    S.g $ do
      S.path
        ! A.fill "none"
        ! A.d bubble
      abc
  where
    bubble = mkPath $ do
      m   (-0.56) ( 0.62)
      aa    0.94    0.78  0  True  True   0  (0.78)
      l   (-0.60) ( 0.9)
      S.z
    abc =
      S.text_ "ABC"
        ! A.x          "0"
        ! A.y          "0.18"
        ! A.textAnchor "middle"
        ! A.fontFamily "Verdana"
        ! A.fontSize   "0.57"
        ! A.fontWeight "bold"
        ! A.letterSpacing "0.05"
        ! A.strokeLinejoin "round"
        ! A.strokeLinecap  "round"