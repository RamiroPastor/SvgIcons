{-# LANGUAGE     OverloadedStrings       #-}


{- |
Country flags (only Europe at this moment) and the European Union flag.

All flags are built on a @viewbox "0 0 w h"@ 
where @w@ and @h@ are particular to each flag (according to official ratios)

Flags are named with the 2-letter ISO code of each country, with very few exceptions, 
named with the 3-letter ISO code to avoid name collision with some HTML or `Attribute` functions 
like `hr` or `cy`.
-}
module SvgIcons.Images.CountryFlags where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Geometry
import SvgIcons.Core.Utils
import SvgIcons.Images.CountryFlagsCoAs


{- |
A list with all the flags of this module, 
together with appropriate names.

>countryFlags :: [ (String , S.Svg) ]
>countryFlags =
>  [ (,) "ad" ad
>  , (,) "af" af
>  , (,) "al" al
>  , (,) "at" at
>  , (,) "ba" ba
>  , (,) "be" be
>  , (,) "bf" bf
>  , (,) "bg" bg
>  , (,) "bj" bj
>  , (,) "by" blr
>  , (,) "cd" cd
>  , (,) "cf" cf
>  , (,) "cg" cg
>  , (,) "ch" ch
>  , (,) "ci" ci
>  , (,) "cm" cm
>  , (,) "cv" cv
>  , (,) "cy" cyp
>  , (,) "cz" cz
>  , (,) "de" de
>  , (,) "dj" dj
>  , (,) "dk" dk
>  , (,) "dz" dz
>  , (,) "ee" ee
>  , (,) "eg" eg
>  , (,) "er" er
>  , (,) "es" es
>  , (,) "et" et
>  , (,) "eu" eu
>  , (,) "fi" fi
>  , (,) "fr" fr
>  , (,) "ga" ga
>  , (,) "gh" gh
>  , (,) "gm" gm
>  , (,) "gn" gn
>  , (,) "gq" gq
>  , (,) "gr" gr
>  , (,) "gw" gw
>  , (,) "hr" hrv
>  , (,) "ie" ie
>  , (,) "is" is
>  , (,) "it" it
>  , (,) "li" li
>  , (,) "lr" lbr
>  , (,) "lt" lt
>  , (,) "lu" lu
>  , (,) "lv" lv
>  , (,) "ly" ly
>  , (,) "ma" ma
>  , (,) "mc" mc
>  , (,) "md" md
>  , (,) "me" me
>  , (,) "mk" mk
>  , (,) "ml" ml
>  , (,) "mr" mrt
>  , (,) "mt" mt
>  , (,) "ne" ne
>  , (,) "ng" ng
>  , (,) "nl" nl
>  , (,) "no" no
>  , (,) "pl" pl
>  , (,) "pt" pt
>  , (,) "ro" ro
>  , (,) "rs" rs
>  , (,) "ru" ru
>  , (,) "sd" sd
>  , (,) "se" se
>  , (,) "si" si
>  , (,) "sk" sk
>  , (,) "sl" sl
>  , (,) "sm" sm
>  , (,) "sn" sn
>  , (,) "so" so
>  , (,) "ss" ss
>  , (,) "st" st
>  , (,) "td" td
>  , (,) "tg" tg
>  , (,) "tn" tn
>  , (,) "ua" ua
>  , (,) "ug" ug
>  , (,) "uk" uk
>  , (,) "va" va
>  , (,) "xk" xk
>  ]
-}
countryFlags :: [ (String , S.Svg) ]
countryFlags =
  [ (,) "ad" ad
  , (,) "af" af
  , (,) "al" al
  , (,) "at" at
  , (,) "ba" ba
  , (,) "be" be
  , (,) "bf" bf
  , (,) "bg" bg
  , (,) "bj" bj
  , (,) "by" blr
  , (,) "cd" cd
  , (,) "cf" cf
  , (,) "cg" cg
  , (,) "ch" ch
  , (,) "ci" ci
  , (,) "cm" cm
  , (,) "cv" cv
  , (,) "cy" cyp
  , (,) "cz" cz
  , (,) "de" de
  , (,) "dj" dj
  , (,) "dk" dk
  , (,) "dz" dz
  , (,) "ee" ee
  , (,) "eg" eg
  , (,) "er" er
  , (,) "es" es
  , (,) "et" et
  , (,) "eu" eu
  , (,) "fi" fi
  , (,) "fr" fr
  , (,) "ga" ga
  , (,) "gh" gh
  , (,) "gm" gm
  , (,) "gn" gn
  , (,) "gq" gq
  , (,) "gr" gr
  , (,) "gw" gw
  , (,) "hr" hrv
  , (,) "ie" ie
  , (,) "is" is
  , (,) "it" it
  , (,) "li" li
  , (,) "lr" lbr
  , (,) "lt" lt
  , (,) "lu" lu
  , (,) "lv" lv
  , (,) "ly" ly
  , (,) "ma" ma
  , (,) "mc" mc
  , (,) "md" md
  , (,) "me" me
  , (,) "mk" mk
  , (,) "ml" ml
  , (,) "mr" mrt
  , (,) "mt" mt
  , (,) "ne" ne
  , (,) "ng" ng
  , (,) "nl" nl
  , (,) "no" no
  , (,) "pl" pl
  , (,) "pt" pt
  , (,) "ro" ro
  , (,) "rs" rs
  , (,) "ru" ru
  , (,) "sd" sd
  , (,) "se" se
  , (,) "si" si
  , (,) "sk" sk
  , (,) "sl" sl
  , (,) "sm" sm
  , (,) "sn" sn
  , (,) "so" so
  , (,) "ss" ss
  , (,) "st" st
  , (,) "td" td
  , (,) "tg" tg
  , (,) "tn" tn
  , (,) "ua" ua
  , (,) "ug" ug
  , (,) "uk" uk
  , (,) "va" va
  , (,) "xk" xk
  ]



{- |
Handy function to draw a flag with 3 vertical stripes of the same size.
-}
flagV3Eq 
  :: (Float,Float) -- ^ @w@ and @h@ parameters for the viewbox
  -> String        -- ^ color for the left stripe
  -> String        -- ^ color for the central stripe
  -> String        -- ^ color for the right stripe
  -> Svg           -- ^ resulting flag
flagV3Eq (w,h) c1 c2 c3 =
    S.svg
      ! A.viewbox (S.toValue $ "0 0 " ++ show w ++ " " ++ show h)
      ! (A.width  .: 300)
      ! (A.height .: 300*h/w)
      $ do
        leftStripe
        centralStripe
        rightStripe
  where
    leftStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: w/3)
        ! (A.height .: h)
        ! A.stroke "none"
        ! A.fill (S.toValue c1)
    centralStripe =
      S.rect
        ! (A.x .: w/3)
        ! (A.y .: 0)
        ! (A.width  .: w/3)
        ! (A.height .: h)
        ! A.stroke "none"
        ! A.fill (S.toValue c2)
    rightStripe = 
      S.rect
        ! (A.x .: 2*w/3)
        ! (A.y .: 0)
        ! (A.width  .: w/3)
        ! (A.height .: h)
        ! A.stroke "none"
        ! A.fill (S.toValue c3)



{- |
Handy function to draw a flag with 3 horizontal stripes of the same size.
-}
flagH3Eq
  :: (Float,Float) -- ^ @w@ and @h@ parameters for the viewbox
  -> String        -- ^ color for the top stripe
  -> String        -- ^ color for the central stripe
  -> String        -- ^ color for the bottom stripe
  -> Svg           -- ^ resulting flag
flagH3Eq (w,h) c1 c2 c3 =
    S.svg
      ! A.viewbox (S.toValue $ "0 0 " ++ show w ++ " " ++ show h)
      ! (A.width  .: 300)
      ! (A.height .: 300*h/w)
      $ do
        topStripe
        midStripe
        botStripe
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: w)
        ! (A.height .: h/3)
        ! A.stroke "none"
        ! A.fill (S.toValue c1)
    midStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: h/3)
        ! (A.width  .: w)
        ! (A.height .: h/3)
        ! A.stroke "none"
        ! A.fill (S.toValue c2)
    botStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 2*h/3)
        ! (A.width  .: w)
        ! (A.height .: h/3)
        ! A.stroke "none"
        ! A.fill (S.toValue c3)


--------------------------------------------------------------------------------

{- |
Flag of Andorra

![flag of Andorra](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ad.svg)
-}
ad :: Svg
ad = 
    S.svg
      ! A.viewbox "0 0 1000 700"
      ! A.width  "300"
      ! A.height "210"
      $ do
        leftStripe
        centreStripe
        rightStripe
        adCoA
  where
    leftStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 320)
        ! (A.height .: 700)
        ! A.stroke "none"
        ! A.fill "#10069F"
    centreStripe =
      S.rect
        ! (A.x .: 320)
        ! (A.y .: 0)
        ! (A.width  .: 360)
        ! (A.height .: 700)
        ! A.stroke "none"
        ! A.fill "#FEDD00"
    rightStripe =
      S.rect
        ! (A.x .: 680)
        ! (A.y .: 0)
        ! (A.width  .: 320)
        ! (A.height .: 700)
        ! A.stroke "none"
        ! A.fill "#D50032"



{- |
Flag of Afghanistan

![flag of Afghanistan](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/af.svg)
-}
af :: Svg
af =
  flagV3Eq
    (3,2)
    "rgb(0,0,0)"
    "rgb(190,0,0)"
    "rgb(0,122,54)"



{- |
Flag of Albania

![flag of Albania](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/al.svg)
-}
al :: Svg
al =
    S.svg
      ! A.viewbox "0 0 980 700"
      ! A.width  "300"
      ! A.height "214.285"
      $ do
        background
        alCoA
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 980)
        ! (A.height .: 700)
        ! A.stroke "none"
        ! A.fill "#FF0000"



{- |
Flag of Austria

![flag of Austria](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/at.svg)
-}
at :: Svg
at = 
  flagH3Eq
    (3,2)
    "#C8102E"
    "#FFFFFF"
    "#C8102E"



{- |
Flag of Bosnia and Herzegovina

![flag of Bosnia and Herzegovina](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ba.svg)
-}
ba :: Svg
ba = 
    S.svg
      ! A.viewbox "0 0 400 200"
      ! A.width  "300"
      ! A.height "150"
      $ do
        defs $ 
          starDef
        background
        triangle
        S.g $ do
          star
          star ! A.transform (translate  25  25)
          star ! A.transform (translate  50  50)
          star ! A.transform (translate  75  75)
          star ! A.transform (translate 100 100)
          star ! A.transform (translate 125 125)
          star ! A.transform (translate 150 150)
          star ! A.transform (translate 175 175)
          star ! A.transform (translate 200 200)
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 400)
        ! (A.height .: 200)
        ! A.stroke "none"
        ! A.fill "#001489"
    triangle =
      S.path
        ! A.fill "#FFCD00"
        ! A.stroke "none"
        ! A.strokeWidth "0"
        ! A.d triangleDirs
    triangleDirs = mkPath $ do
      m   106    0
      l   306    0
      l   306  200
      S.z
    a = (19 * (sqrt 5) - 38) / 2
    starDef =
      starRegular 5 19 (68,-a)
        ! A.fill "#FFFFFF"
        ! A.strokeWidth "0"
        ! A.id_ "HaskellSvgIcons-baFlagStar"
    star =
      S.use 
        ! A.fill "#FFFFFF"
        ! A.xlinkHref "#HaskellSvgIcons-baFlagStar"



{- |
Flag of Belgium

![flag of Belgium](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/be.svg)
-}
be :: Svg
be =
  flagV3Eq
    (3,2.6)
    "#000000"
    "#FFE936"
    "#FF0F21"



{- |
Flag of Burkina Faso

![flag of Burkina Faso](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/bf.svg)
-}
bf :: Svg
bf =
    S.svg
      ! A.viewbox "0 0 18 12"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topBand
        botBand
        star
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "18"
        ! A.height "6"
        ! A.fill "#EF2B2D"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "6"
        ! A.width  "18"
        ! A.height "6"
        ! A.fill "#009E49"
    star =
      starRegular 5 2 (9,6)
        ! A.fill "#FCD116"




{- |
Flag of Bulgaria

![flag of Bulgaria](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/bg.svg)
-}
bg :: Svg
bg =
  flagH3Eq
    (5,3)
    "#FFFFFF"
    "#009B74"
    "#D01C1F"



{- |
Flag of Benin

![flag of Benin](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/bj.svg)
-}
bj :: Svg
bj =
    S.svg
      ! A.viewbox "0 0 300 200"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topBand
        botBand
        leftStripe
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "300"
        ! A.height "100"
        ! A.fill "#FCD20F"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "100"
        ! A.width  "300"
        ! A.height "100"
        ! A.fill "#E90929"
    leftStripe =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "120"
        ! A.height "200"
        ! A.fill "#008850"



{- |
Flag of Belarus

![flag of Belarus](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/by.svg)
-}
blr :: Svg
blr = 
    S.svg
      ! A.viewbox "0 0 90 45"
      ! A.width  "300"
      ! A.height "150"
      $ do
        topStripe
        botStripe
        whiteStripe
        ruchnik
  where
    topStripe =
      S.rect
        ! (A.x .: 10)
        ! (A.y .:  0)
        ! (A.width  .: 80)
        ! (A.height .: 30)
        ! A.stroke "none"
        ! A.fill "#CF101A"
    botStripe =
      S.rect
        ! (A.x .: 10)
        ! (A.y .: 30)
        ! (A.width  .: 80)
        ! (A.height .: 15)
        ! A.stroke "none"
        ! A.fill "#007D2C"
    whiteStripe =
      S.rect
        ! (A.x .: 1)
        ! (A.y .: 0)
        ! (A.width  .:  9)
        ! (A.height .: 45)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    ruchnikMatrix =
      [ [0,0,0,0,1,1,1,0,0,0,0,1]
      , [1,0,0,1,1,1,1,1,0,0,0,1]
      , [0,0,1,1,1,0,1,1,1,0,0,0]
      , [0,1,1,1,0,0,0,1,1,1,0,0]
      , [1,1,1,0,0,1,0,0,1,1,1,0] -- center 1
      , [0,1,1,1,0,0,0,1,1,1,0,0]
      , [0,0,1,1,1,0,1,1,1,0,0,0]
      , [1,0,0,1,1,1,1,1,0,0,0,1]
      , [0,0,0,0,1,1,1,0,0,0,0,1]
      , [0,0,1,0,0,1,0,0,1,0,0,0]
      , [0,1,1,1,0,0,0,1,1,1,0,0]
      , [1,1,0,1,1,0,1,1,0,1,1,0] -- center 2
      , [0,1,1,1,0,0,0,1,1,1,0,0]
      , [0,0,1,0,0,1,0,0,1,0,0,0]
      , [0,0,0,0,1,1,1,0,0,0,0,1]
      , [1,0,0,1,1,1,1,1,0,0,0,1]
      , [0,0,1,1,1,1,1,1,1,0,0,0]
      , [0,1,1,1,1,1,1,1,1,1,0,0]
      , [1,1,1,1,1,1,1,1,1,1,1,0]
      , [1,1,1,1,0,0,0,1,1,1,1,1] -- center 3
      , [0,1,1,1,1,1,0,0,1,1,1,1]
      , [0,0,1,1,1,0,0,0,0,1,1,1]
      , [1,0,0,1,0,0,0,0,1,1,1,1]
      , [0,0,0,0,0,0,0,1,1,1,1,0]
      , [1,0,0,0,0,0,1,1,1,1,0,0]
      , [1,1,0,0,0,1,1,1,1,0,0,1]
      , [1,1,1,0,1,1,1,1,0,0,0,0]
      , [0,1,1,1,1,1,1,0,0,1,0,0]
      , [0,0,1,1,1,1,0,0,0,1,1,0]
      , [0,0,0,1,1,1,0,0,0,0,1,1]
      , [0,0,0,0,1,1,0,1,0,0,0,1]
      ]
    w = 10 / 23
    h = 45 / 61
    ruchnik =
      S.path
        ! A.fill "none"
        ! A.stroke "#CF101A"
        ! (A.strokeWidth .: w)
        ! A.d ruchnikDirs
    ruchnikDirs = mkPath $ do
      mapM_ 
        (\n -> drawLine (fromIntegral n) $ ruchnikMatrix !! n) 
        [0 .. 30]
      mapM_ 
        (\n -> drawLine (fromIntegral n) $ ruchnikMatrix !! (60 - n)) 
        [31 .. 60]
    drawLine n binL = do
      mapM_ 
        (\k ->
          if 0 == binL !! (fromEnum k)
            then return ()
            else m  ( 0 + (fromIntegral k)*w + w/2)  (n*h)  >>  vr h
        ) [0 .. 10]
      if 0 == binL !! 11 
        then return ()
        else m 5 (n*h) >> vr h
      mapM_ 
        (\k ->
          if 0 == binL !! k
            then return ()
            else m  (10 - (fromIntegral k)*w - w/2)  (n*h)  >>  vr h
        ) [0 .. 10]



{- |
Flag of the Democratic Republic of the Congo

![flag of the Democratic Republic of the Congo](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/cd.svg)
-}
cd :: Svg
cd =
    S.svg
      ! A.viewbox "0 0 800 600"
      ! A.width  "300"
      ! A.height "225"
      $ do
        back
        yellowBand
        redBand
        star
  where
    back =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "800"
        ! A.height "600"
        ! A.fill "#007FFF"
    yellowBand =
      S.path
        ! A.fill "#F7D618"
        ! A.d yellowDirs
    yellowDirs = mkPath $ do
      m   750    0
      l     0  450
      l     0  600
      l    50  600
      l   800  150
      l   800    0
      S.z
    redBand =
      S.path
        ! A.fill "#CE1021"
        ! A.d redDirs
    redDirs = mkPath $ do
      m   800    0
      l     0  480
      l     0  600
      l   800  120
      S.z
    r1 = 220 / (1 + cos(pi/5))
    star = 
      starRegular 5 r1 (146, 36 + r1)
        ! A.fill "#F7D618"



{- |
Flag of the Central African Republic

![flag of the Central African Republic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/cf.svg)
-}
cf :: Svg
cf =
    S.svg
      ! A.viewbox "0 0 60 40"
      ! A.width  "300"
      ! A.height "200"
      $ do
        band1
        band2
        band3
        band4
        centralStripe
        star
  where
    band1 =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "60"
        ! A.height "10"
        ! A.fill "#003082"
    band2 =
      S.rect
        ! A.x "0"
        ! A.y "10"
        ! A.width  "60"
        ! A.height "10"
        ! A.fill "#FFFFFF"
    band3 =
      S.rect
        ! A.x "0"
        ! A.y "20"
        ! A.width  "60"
        ! A.height "10"
        ! A.fill "#289728"
    band4 =
      S.rect
        ! A.x "0"
        ! A.y "30"
        ! A.width  "60"
        ! A.height "10"
        ! A.fill "#FFCE00"
    centralStripe =
      S.rect
        ! A.x "25"
        ! A.y "0"
        ! A.width  "10"
        ! A.height "40"
        ! A.fill "#D21034"
    a = (107 - 9 * sqrt 5) / 16
    star =
      starRegular 5 4.5 (10 , a)
        ! A.fill "#FFCE00"



{- |
Flag of Congo

![flag of Congo](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/cg.svg)
-}
cg :: Svg
cg =
    S.svg
      ! A.viewbox "0 0 300 200"
      ! A.width  "300"
      ! A.height "200"
      $ do
        leftTriangle
        midBand
        rightTriangle
  where
    leftTriangle =
      S.path
        ! A.fill "#009543"
        ! A.d leftDirs
    midBand =
      S.path
        ! A.fill "#FBDE4A"
        ! A.d midDirs
    rightTriangle =
      S.path
        ! A.fill "#DC241F"
        ! A.d rightDirs
    leftDirs = mkPath $ do
      m    0   0
      l  200   0
      l    0 200
      S.z
    midDirs = mkPath $ do
      m  200   0
      l  300   0
      l  100 200
      l    0 200
      S.z
    rightDirs = mkPath $ do
      m  300   0
      l  300 200
      l  100 200
      S.z



{- |
Flag of Switzerland

![flag of Switzerland](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ch.svg)
-}
ch :: S.Svg
ch =
    S.svg
      ! A.viewbox "0 0 32 32"
      ! A.width  "300"
      ! A.height "300"
      $ do
        background
        cross
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 32)
        ! (A.height .: 32)
        ! A.stroke "none"
        ! A.fill "#FF0000"
    cross =
      S.path
        ! A.fill "none"
        ! A.stroke "#FFFFFF"
        ! A.strokeWidth "6"
        ! A.d crossDirs
    crossDirs = mkPath $ do
      m  16  6
      l  16 26
      m   6 16
      l  26 16



{- |
Flag of Côte d'Ivoire

![flag of Côte d'Ivoire](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ci.svg)
-}
ci :: Svg
ci =
  flagV3Eq
    (3,2)
    "#FF8200"
    "#FFFFFF"
    "#009A44"



{- |
Flag of Cameroon

![flag of Cameroon](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/cm.svg)
-}
cm :: Svg
cm =
    S.svg
      ! A.viewbox "0 0 300 200"
      ! A.width  "300"
      ! A.height "200"
      $ do
        leftBand
        midBand
        rightBand
        star
  where
    leftBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "100"
        ! A.height "200"
        ! A.fill "#007A5E"
    midBand =
      S.rect
        ! A.x "100"
        ! A.y "0"
        ! A.width  "100"
        ! A.height "200"
        ! A.fill "#CE1126"
    rightBand =
      S.rect
        ! A.x "200"
        ! A.y "0"
        ! A.width  "100"
        ! A.height "200"
        ! A.fill "#FCD116"
    star =
      starRegular 5 27 (150,100)
        ! A.fill "#FCD116"



{- |
Flag of Cabo Verde

![flag of Cabo Verde](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/cv.svg)
-}
cv :: Svg
cv =
    S.svg
      ! A.viewbox "0 0 450 300"
      ! A.width  "300"
      ! A.height "200"
      $ do
        blueBand
        whiteBand
        redBand
        starCircle
  where
    blueBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "450"
        ! A.height "300"
        ! A.fill "#003893"
    whiteBand =
      S.rect
        ! A.x "0"
        ! A.y "150"
        ! A.width  "450"
        ! A.height "75"
        ! A.fill "#FFFFFF"
    redBand  =
      S.rect
        ! A.x "0"
        ! A.y "175"
        ! A.width  "450"
        ! A.height "25"
        ! A.fill "#CF2027"
    star (c1,c2) =
      starRegular 5 12.5 (c1,c2)
        ! A.fill "#F7D116"
    getCentre k =
      ( 168.75 - 75 * sin (k * 2*pi / 10)
      , 187.5  + 75 * cos (k * 2*pi / 10)
      )
    starCircle =
      S.g $ mapM_ (star . getCentre . fromIntegral) [0 .. 9]



{- |
Flag of Cyprus

![flag of Cyprus](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/cy.svg)
-}
cyp :: Svg
cyp =
    S.svg
      ! A.viewbox "0 0 900 600"
      ! A.width  "300"
      ! A.height "200"
      $ do
        background
        cyCoA
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 900)
        ! (A.height .: 600)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"



{- |
Flag of Czech Republic

![flag of Czech Republic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/cz.svg)
-}
cz :: S.Svg
cz =
    S.svg
      ! A.viewbox "0 0 6 4"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topStripe
        leftTriangle
        botStripe
  where
    topStripe =
      S.path 
        ! A.strokeWidth "0"
        ! A.fill "#FFFFFF"
        ! A.d topDirs
    topDirs = mkPath $ do
      m  0 0
      l  6 0
      l  6 2
      l  3 2
      S.z
    leftTriangle =
      S.path
        ! A.strokeWidth "0"
        ! A.fill "#11457E"
        ! A.d triangleDirs
    triangleDirs = mkPath $ do
      m  0 0
      l  3 2
      l  0 4
      S.z
    botStripe =
      S.path
        ! A.strokeWidth "0"
        ! A.fill "#D7141A"
        ! A.d botDirs
    botDirs = mkPath $ do
      m  0 4
      l  6 4
      l  6 2
      l  3 2
      S.z



{- |
Flag of Germany

![flag of Germany](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/de.svg)
-}
de :: Svg
de =
  flagH3Eq
    (5,3)
    "rgb(0,0,0)"
    "rgb(255,0,0)"
    "rgb(255,204,0)"



{- |
Flag of Djibouti

![flag of Djibouti](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/dj.svg)
-}
dj :: Svg
dj =
    S.svg
      ! A.viewbox "0 0 300 200"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topBand
        botBand
        leftTriangle
        star
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "300"
        ! A.height "100"
        ! A.fill "#6AB2E7"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "100"
        ! A.width  "300"
        ! A.height "100"
        ! A.fill "#12AD2B"
    leftTriangle =
      S.path
        ! A.fill "#FFFFFF"
        ! A.d triangleDirs
    triangleDirs = mkPath $ do
      m   0   0
      l  (100 * tan(pi/3))  100
      l   0   200
      S.z
    star =
      starRegular 5 (100/3) (100 * tan(pi/6) , 100)
        ! A.fill "#D7141A"



{- |
Flag of Denmark

![flag of Denmark](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/dk.svg)
-}
dk :: Svg
dk =
    S.svg
      ! A.viewbox "0 0 37 28"
      ! A.width  "300"
      ! A.height "227"
      $ do
        background
        cross
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 37)
        ! (A.height .: 28)
        ! A.stroke "none"
        ! A.fill "#C8102E"
    cross =
      S.path
        ! A.fill "none"
        ! A.stroke "#FFFFFF"
        ! A.strokeWidth "4"
        ! A.d crossDirs
    crossDirs = mkPath $ do
      m  14  0
      l  14 28
      m   0 14
      l  37 14



{- |
Flag of Algeria

![flag of Algeria](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/dz.svg)
-}
dz :: Svg
dz =
    S.svg
      ! A.viewbox "0 0 30 20"
      ! A.width  "300"
      ! A.height "200"
      $ do
        leftBand
        rightBand
        moon
        star
  where
    leftBand = 
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "15"
        ! A.height "20"
        ! A.fill "#006633"
    rightBand =
      S.rect
        ! A.x "15"
        ! A.y "0"
        ! A.width  "15"
        ! A.height "20"
        ! A.fill "#FFFFFF"
    x1 = 2.5 / tan (pi/6)
    y1 = 2.5
    r1 = 5
    r2 = 4
    moon =
      S.path
        ! A.fill "#D21034"
        ! A.d moonDirs
    moonDirs = mkPath $ do
      m   (15 + x1)  (10 - y1)
      aa  r1   r1   0   True  False (15 + x1)  (10 + y1)
      aa  r2   r2   0   True  True  (15 + x1)  (10 - y1)
      S.z
    b = (5 + 5 * sqrt 5) / 8
    star = 
      starRegular 5 2.5 (15 + b, 10)
        ! A.fill "#D21034"
        ! A.transform (rotateAround 90 (15 + b) 10)



{- |
Flag of Estonia

![flag of Estonia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ee.svg)
-}
ee :: Svg
ee =
  flagH3Eq
    (5.5, 3.5)
    "#0072CE"
    "#000000"
    "#FFFFFF"



{- |
Flag of Egypt

![flag of Egypt](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/eg.svg)
-}
eg :: Svg
eg =
    S.svg
      ! A.viewbox "0 0 900 600"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topBand
        midBand
        botBand
        egCoA
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "900"
        ! A.height "200"
        ! A.fill "#CE1126"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "200"
        ! A.width  "900"
        ! A.height "200"
        ! A.fill "#FFFFFF"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "400"
        ! A.width  "900"
        ! A.height "200"
        ! A.fill "#000000"



{- |
Flag of Eritrea

![flag of Eritrea](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/er.svg)
-}
er :: Svg
er =
    S.svg
      ! A.viewbox "0 0 1200 600"
      ! A.width  "300"
      ! A.height "150"
      $ do
        topBand
        botBand
        redTriangle
        erCoA
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "1200"
        ! A.height " 300"
        ! A.fill "#0BAC24"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "300"
        ! A.width  "1200"
        ! A.height " 300"
        ! A.fill "#3C8BDC"
    redTriangle =
      S.path
        ! A.fill "#EB0433"
        ! A.d triangleDirs
    triangleDirs = mkPath $ do
      m     0    0
      l  1200  300
      l     0  600
      S.z



{- |
Flag of Spain

![flag of Spain](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/es.svg)
-}
es :: Svg
es =
    S.svg
      ! A.viewbox "0 0 750 500"
      ! A.width  "300"
      ! A.height "200"
      $ do
        redBand
        yellowBand
  where
    colRed = "rgb(198,11,30)"
    colYellow = "rgb(255,196,0)"
    redBand =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 750)
        ! (A.height .: 500)
        ! A.stroke "none"
        ! A.fill colRed
    yellowBand =
      S.rect 
        ! (A.x .: 0)
        ! (A.y .: 125)
        ! (A.width  .: 750)
        ! (A.height .: 250)
        ! A.stroke "none"
        ! A.fill colYellow



{- |
Flag of the Ethiopia

![flag of the Ethiopia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/et.svg)
-}
et :: Svg
et =
    S.svg
      ! A.viewbox "0 0 1200 600"
      ! A.width  "300"
      ! A.height "150"
      $ do
        topBand
        midBand
        botBand
        etCoA
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "1200"
        ! A.height "200"
        ! A.fill "#078930"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "200"
        ! A.width  "1200"
        ! A.height "200"
        ! A.fill "#FCDD09"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "400"
        ! A.width  "1200"
        ! A.height "200"
        ! A.fill "#DA121A"



{- |
Flag of the European Union

![flag of the European Union](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/eu.svg)
-}
eu :: Svg
eu =
    S.svg
      ! A.viewbox "0 0 3 2"
      ! A.width  "300"
      ! A.height "200"
      $ do
        background
        mapM_ star [0..11]
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 3)
        ! (A.height .: 2)
        ! A.stroke "none"
        ! A.fill "#003399"
    starPos k = 
      ( 3/2 + (2/3) * cos (k*pi/6) 
      , 1   + (2/3) * sin (k*pi/6)
      )
    star k =
      starRegular 5 (1/9) (starPos k)
        ! A.fill "#FFCC00"
        ! A.id_ "HaskellSvgIcons-euFlagStar"



{- |
Flag of Finland

![flag of Finland](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/fi.svg)
-}
fi :: S.Svg
fi =
    S.svg
      ! A.viewbox "0 0 36 22"
      ! A.width  "300"
      ! A.height "183.33"
      $ do
        background
        cross
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 36)
        ! (A.height .: 22)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    cross =
      S.path
        ! A.fill "none"
        ! A.stroke "#002F6C"
        ! A.strokeWidth "6"
        ! A.d crossDirs
    crossDirs = mkPath $ do
      m  13  0
      l  13 26
      m   0 11
      l  36 11



{- |
Flag of France

![flag of France](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/fr.svg)
-}
fr :: S.Svg
fr =
  flagV3Eq
    (3,2)
    "rgb(0,85,164)"
    "rgb(255,255,255"
    "rgb(239,65,53)"



{- |
Flag of Gabon

![flag of Gabon](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ga.svg)
-}
ga :: Svg
ga =
  flagH3Eq
    (4,3)
    "#009E60"
    "#FCD116"
    "#3A75C4"



{- |
Flag of Ghana

![flag of Ghana](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/gh.svg)
-}
gh :: Svg
gh =
    S.svg
      ! A.viewbox "0 0 18 12"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topBand
        midBand
        botBand
        star
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "18"
        ! A.height "4"
        ! A.fill "#CF0921"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "4"
        ! A.width  "18"
        ! A.height "4"
        ! A.fill "#FCD20F"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "8"
        ! A.width  "18"
        ! A.height "4"
        ! A.fill "#006B3D"
    d = 8 - (8/5) * sqrt 5
    a = d/2 - 2
    star =
      starRegular 5 (d/2) (9 , 6+a)
        ! A.fill "#000000"




{- |
Flag of Gambia

![flag of Gambia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/gm.svg)
-}
gm :: Svg
gm = 
    S.svg
      ! A.viewbox "0 0 27 18"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topBand
        midBand
        botBand
        blueBand
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "27"
        ! A.height "6"
        ! A.fill "#CE1126"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "6"
        ! A.width  "27"
        ! A.height "6"
        ! A.fill "#FFFFFF"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "12"
        ! A.width  "27"
        ! A.height "6"
        ! A.fill "#3A7728"
    blueBand =
      S.rect
        ! A.x "0"
        ! A.y "7"
        ! A.width  "27"
        ! A.height "4"
        ! A.fill "#0C1C8C"



{- |
Flag of Guinea (Conakry)

![flag of Guinea (Conakry)](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/gn.svg)
-}
gn :: Svg
gn =
  flagV3Eq
    (3,2)
    "#CE1126"
    "#FCD116"
    "#009460"



{- |
Flag of Equatorial Guinea

![flag of Equatorial Guinea](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/gq.svg)
-}
gq :: Svg
gq =
    S.svg
      ! A.viewbox "0 0 3600 2400"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topBand
        midBand
        botBand
        leftTriangle
        gqCoA
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "3600"
        ! A.height "800"
        ! A.fill "#3E9A00"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "800"
        ! A.width  "3600"
        ! A.height "800"
        ! A.fill "#FFFFFF"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "1600"
        ! A.width  "3600"
        ! A.height "800"
        ! A.fill "#E32118"
    leftTriangle =
      S.path
        ! A.fill "#0073CE"
        ! A.d triangleDirs
    triangleDirs = mkPath $ do
      m    0    0
      l  900 1200
      l    0 2400
      S.z



{- |
Flag of Greece

![flag of Greece](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/gr.svg)
-}
gr :: Svg
gr =
    S.svg
      ! A.viewbox "0 0 27 18"
      ! A.width  "300"
      ! A.height "200"
      $ do
        blueLines
        whiteLines
        blueSquare
        greekCross
  where
    blueLines =
      S.path
        ! A.fill "none"
        ! A.stroke "#004C98"
        ! A.strokeWidth "2"
        ! A.d blueDirs
    whiteLines =
      S.path
        ! A.fill "none"
        ! A.stroke "#FFFFFF"
        ! A.strokeWidth "2"
        ! A.d whiteDirs
    blueDirs = mkPath $ do
      m  0  1  >>  hr 27
      m  0  5  >>  hr 27
      m  0  9  >>  hr 27
      m  0 13  >>  hr 27
      m  0 17  >>  hr 27
    whiteDirs = mkPath $ do
      m  0  3  >>  hr 27
      m  0  7  >>  hr 27
      m  0 11  >>  hr 27
      m  0 15  >>  hr 27
    blueSquare =
      S.rect 
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 10)
        ! (A.height .: 10)
        ! A.stroke "none"
        ! A.fill "#004C98"
    greekCross =
      S.path
        ! A.fill "none"
        ! A.stroke "#FFFFFF"
        ! A.strokeWidth "2"
        ! A.d crossDirs
    crossDirs = mkPath $ do
      m   5   0
      l   5  10
      m   0   5
      l  10   5



{- |
Flag of Guinea Bissau

![flag of Guinea Bissau](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/gw.svg)
-}
gw :: Svg
gw =
    S.svg
      ! A.viewbox "0 0 12 6"
      ! A.width  "300"
      ! A.height "150"
      $ do
        topBand
        botBand
        leftBand
        star
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "12"
        ! A.height "3"
        ! A.fill "#FCD116"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "3"
        ! A.width  "12"
        ! A.height "3"
        ! A.fill "#009E49"
    leftBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "4"
        ! A.height "6"
        ! A.fill "#CE1126"
    star =
      starRegular 5 1 (2,3)
        ! A.fill "#000000"



{- |
Flag of Croatia

![flag of Croatia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/hr.svg)
-}
hrv :: Svg
hrv =
   S.svg
    ! A.viewbox "0 0 1200 600"
    ! A.width  "300"
    ! A.height "150"
    $ do
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.height "200"
        ! A.width  "1200"
        ! A.fill   "#FF0000"
      S.rect
        ! A.x "0"
        ! A.y "200"
        ! A.height "200"
        ! A.width  "1200"
        ! A.fill   "#FFFFFF"
      S.rect
        ! A.x "0"
        ! A.y "400"
        ! A.height "200"
        ! A.width  "1200"
        ! A.fill   "#171796"
      hrCoA



{- |
Flag of Ireland

![flag of Ireland](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ie.svg)
-}
ie :: S.Svg
ie =
  flagV3Eq
    (3,1.5)
    "rgb(22,155,98)"
    "rgb(255,255,255)"
    "rgb(255,136,62)"



{- |
Flag of Iceland

![flag of Iceland](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/is.svg)
-}
is :: S.Svg
is = 
    S.svg 
      ! A.viewbox "0 0 25 18"
      ! A.width  "300"
      ! A.height "216"
      $ do
        background
        whiteCross
        redCross
  where
    background =
      S.rect 
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 25)
        ! (A.height .: 18)
        ! A.stroke "none"
        ! A.fill "#02529C"
    whiteCross =
      S.path 
        ! A.fill "none"
        ! A.strokeWidth "4"
        ! A.stroke "#FFFFFF"
        ! A.d crossDirs
    redCross =
      S.path 
        ! A.fill "none"
        ! A.strokeWidth "2"
        ! A.stroke "#DC1E35"
        ! A.d crossDirs
    crossDirs = mkPath $ do
      m   0   9
      l  25   9
      m   9   0
      l   9  18



{- |
Flag of Italy

![flag of Italy](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/it.svg)
-}
it :: Svg
it = 
  flagV3Eq
    (3,2)
    "rgb(0,140,69)"
    "rgb(244,249,255"
    "rgb(205,33,42)" 



{- |
Flag of Liechtenstein

![flag of Liechtenstein](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/li.svg)
-}
li :: Svg
li = 
    S.svg
      ! A.viewbox "0 0 1000 600"
      ! A.width  "300"
      ! A.height "180"
      $ do
        topStripe
        botStripe
        liCoA
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 1000)
        ! (A.height .:  300)
        ! A.stroke "none"
        ! A.fill "#002780"
    botStripe =
      S.rect
        ! (A.x .:   0)
        ! (A.y .: 300)
        ! (A.width  .: 1000)
        ! (A.height .:  300)
        ! A.stroke "none"
        ! A.fill "#CF0921"



{- |
Flag of Liberia

![flag of Liberia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/lr.svg)
-}
lbr :: Svg
lbr =
    S.svg
      ! A.viewbox "0 0 209 110"
      ! A.width  "300"
      ! A.height "157.894736"
      $ do
        redBackground
        whiteStripes
        blueSquare
        star
  where
    redBackground =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "209"
        ! A.height "110"
        ! A.fill "#B22234"
    whiteStripe ky =
      S.rect
        ! A.x "0"
        ! A.y (S.toValue ky)
        ! A.width  "209"
        ! A.height "10"
        ! A.fill "#FFFFFF"
    whiteStripes =
      S.g $ mapM_ whiteStripe ([10, 30, 50, 70, 90] :: [Int])
    blueSquare =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "50"
        ! A.height "50"
        ! A.fill "#00205B"
    r1 = 30 / (1 + cos(pi/5))
    star =
      starRegular 5 r1 (25,25)
        ! A.fill "#FFFFFF"



{- |
Flag of Lithuania

![flag of Lithuania](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/lt.svg)
-}
lt :: Svg
lt =
  flagH3Eq
    (5,3)
    "#FFB81C"
    "#046A38"
    "#BE3A34"



{- |
Flag of Luxembourg

![flag of Luxembourg](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/lu.svg)
-}
lu :: S.Svg
lu =
  flagH3Eq
    (3,2)
    "#EA141D"
    "#FFFFFF"
    "#51ADDA"



{- |
Flag of Latvia

![flag of Latvia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/lv.svg)
-}
lv :: S.Svg
lv = 
    S.svg
      ! A.viewbox "0 0 20 10"
      ! A.width  "300"
      ! A.height "150"
      $ do
        topStripe
        midStripe
        botStripe
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 20)
        ! (A.height .: 4)
        ! A.stroke "none"
        ! A.fill "#A4343A"
    midStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 4)
        ! (A.width  .: 20)
        ! (A.height .: 2)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    botStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 6)
        ! (A.width  .: 20)
        ! (A.height .: 4)
        ! A.stroke "none"
        ! A.fill "#A4343A"



{- |
Flag of Libya

![flag of Libya](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ly.svg)
-}
ly :: Svg
ly =
    S.svg
      ! A.viewbox "0 0 48 24"
      ! A.width  "300"
      ! A.height "150"
      $ do
        topBand
        midBand
        botBand
        moon
        star
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "48"
        ! A.height "6"
        ! A.fill "#E70013"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "6"
        ! A.width  "48"
        ! A.height "12"
        ! A.fill "#000000"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "18"
        ! A.width  "48"
        ! A.height "6"
        ! A.fill "#239E46"
    x1 = 24 + 3 * cos(pi/4)
    y1 = 12 - 3 * sin(pi/4)
    y2 = 12 + 3 * sin(pi/4)
    a  = 0.5 * sqrt (706 - 480 * sqrt 2)
    c  = (1/5) * (15 - 3 * sqrt 5) * sqrt (5 + sqrt 5)
    d  = 3 + (c/8) * (-1 + sqrt 5)
    moon =
      S.path
        ! A.fill "#FFFFFF"
        ! A.d moonDirs
    moonDirs = mkPath $ do
      m   x1  y1
      aa  3   3   0   True  False x1  y2
      aa  a   a   0   True  True  x1  y1
      S.z
    star =
      starRegular 5 (c/2) (24 + d , 12)
        ! A.fill "#FFFFFF"
        ! A.transform (rotateAround (-90) (24 + d) 12)



{- |
Flag of Morocco

![flag of Morocco](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ma.svg)
-}
ma :: Svg
ma =
    S.svg
      ! A.viewbox "0 0 6 4"
      ! A.width  "300"
      ! A.height "200"
      $ do
        back
        centralStar
  where
    back =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "300"
        ! A.height "200"
        ! A.fill "#C1272D"
    a = 0.5 * sqrt (10 + 2 * sqrt 5)
    b = a / 20
    centralStar =
      starPolygonOverlap 5 1 b (3,2)
        ! A.fill "#006233"
        ! A.stroke "#000000"
        ! A.strokeWidth "0.01"



{- |
Flag of Monaco

![flag of Monaco](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/mc.svg)
-}
mc :: S.Svg
mc =
    S.svg
      ! A.viewbox "0 0 5 4"
      ! A.width  "300"
      ! A.height "240"
      $ do
        topStripe
        botStripe
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 5)
        ! (A.height .: 2)
        ! A.stroke "none"
        ! A.fill "#CE1126"
    botStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 2)
        ! (A.width  .: 5)
        ! (A.height .: 2)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"



{- |
Flag of Moldova

![flag of Moldova](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/md.svg)
-}
md :: Svg
md =
  S.svg
    ! A.viewbox "0 0 1800 900"
    ! A.width  "300"
    ! A.height "150"
    $ do
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "600"
        ! A.height "900"
        ! A.fill   "#003DA5"
      S.rect
        ! A.x "600"
        ! A.y "0"
        ! A.width  "600"
        ! A.height "900"
        ! A.fill   "#FFD100"
      S.rect
        ! A.x "1200"
        ! A.y "0"
        ! A.width  "600"
        ! A.height "900"
        ! A.fill   "#C8102E"
      mdCoA



{- |
Flag of Montenegro

![flag of Montenegro](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/me.svg)
-}
me :: Svg
me = 
    S.svg
      ! A.viewbox "0 0 1200 600"
      ! A.width  "300"
      ! A.height "150"
      $ do
        background
        border
        meCoA
  where
    w = 1200
    h =  600
    s = h / 40
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: w)
        ! (A.height .: h)
        ! A.stroke "none"
        ! A.fill "#FF0000"
    border =
      S.path
        ! A.fill "none"
        ! A.stroke "#E6B319"
        ! (A.strokeWidth .: 2*s)
        ! A.d borderDirs
    borderDirs = mkPath $ do
      m  (0 + s) (0 + s)
      l  (w - s) (0 + s)
      l  (w - s) (h - s)
      l  (0 + s) (h - s)
      S.z



{- |
Flag of North Macedonia

![flag of North Macedonia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/mk.svg)
-}
mk :: Svg
mk =
    S.svg
      ! A.viewbox "0 0 2 1"
      ! A.width  "300"
      ! A.height "150"
      $ do
        background
        rays
        sun
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 2)
        ! (A.height .: 1)
        ! A.stroke "none"
        ! A.fill "#CE2028"
    d = 2/7
    sun =
      S.circle
        ! (A.cx .: 1)
        ! (A.cy .: 0.5)
        ! (A.r  .: d/2)
        ! A.fill "#F9D616"
        ! A.stroke "#CE2028"
        ! (A.strokeWidth .: d/8)
    rays =
      S.path
        ! A.stroke "none"
        ! A.strokeWidth "0"
        ! A.fill "#F9D616"
        ! A.d raysDirs
    x1 = 1 + (1/68) * sqrt (3825 / 98)  -- 1.09187 etc.
    x2 = 1 - (1/68) * sqrt (3825 / 98)
    y1 = (3/5) * x1 - 1/10
    y2 = (3/5) * x2 - 1/10
    raysDirs = mkPath $ do
      m  (1 - 0.1)  (0)
      l  (1 + 0.1)  (0)
      l  (1      )  (0.5 - d/2 + d/8)
      S.z
      m  (1 - 0.1)  (1)
      l  (1 + 0.1)  (1)
      l  (1      )  (0.5 + d/2 - d/8)
      S.z
      m  (0      )  (0.5 - 0.1)
      l  (0      )  (0.5 + 0.1)
      l  (1      )   0.5
      S.z
      m  (2      )  (0.5 - 0.1)
      l  (2      )  (0.5 + 0.1)
      l  (1      )   0.5
      S.z
      m  (0      )   0
      l  (0 + 0.3)   0
      l   x1         y1
      S.z
      m  (2 - 0.3)   0
      l  (2      )   0
      l   x2         y1
      S.z
      m  (2 - 0.3)   1
      l  (2      )   1
      l   x2         y2
      S.z
      m  (0      )   1
      l  (0 + 0.3)   1
      l   x1         y2
      S.z



{- |
Flag of Mali

![flag of Mali](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ml.svg)
-}
ml :: Svg
ml =
  flagV3Eq
    (3,2)
    "#14B53A"
    "#FCD116"
    "#CE1126"



{- |
Flag of Mauritania

![flag of Mauritania](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/mr.svg)
-}
mrt :: Svg
mrt = 
    S.svg
      ! A.viewbox "0 0 1500 1000"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topBand
        midBand
        botBand
        moon
        star
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "1500"
        ! A.height " 200"
        ! A.fill "#D01C1F"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "200"
        ! A.width  "1500"
        ! A.height " 600"
        ! A.fill "#00A95C"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "800"
        ! A.width  "1500"
        ! A.height " 200"
        ! A.fill "#D01C1F"
    x1 = 750 - 375
    x2 = 750 + 375
    y1 = 325
    r1 = 375
    r2 = (273^2 + 375^2) / (2 * 273)
    moon =
      S.path
        ! A.fill "#FFD700"
        ! A.d moonDirs
    moonDirs = mkPath $ do
      m   x1  y1
      aa  r1  r1  0  True  False x2  y1
      aa  r2  r2  0  False True  x1  y1
      S.z 
    r3  = 191 / (1 + cos (pi/5))
    y3  = 300 + r3
    star =
      starRegular 5 r3 (750 , y3)
        ! A.fill "#FFD700"



{- |
Flag of Malta

![flag of Malta](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/mt.svg)
-}
mt :: Svg
mt =
    S.svg
      ! A.viewbox "0 0 900 600"
      ! A.width  "300"
      ! A.height "200"
      $ do
        leftStripe
        rightStripe
        mtCoA
  where
    leftStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 450)
        ! (A.height .: 600)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    rightStripe =
      S.rect
        ! (A.x .: 450)
        ! (A.y .: 0)
        ! (A.width  .: 450)
        ! (A.height .: 600)
        ! A.stroke "none"
        ! A.fill "#C01B22"



{- |
Flag of Niger

![flag of Niger](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ne.svg)
-}
ne :: Svg
ne =
    S.svg
      ! A.viewbox "0 0 7 6"
      ! A.width  "300"
      ! (A.height .: 300 * 6 / 7)
      $ do
        topBand
        midBand
        botBand
        centralCircle
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "7"
        ! A.height "2"
        ! A.fill "#E05206"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "2"
        ! A.width  "7"
        ! A.height "2"
        ! A.fill "#FFFFFF"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "4"
        ! A.width  "7"
        ! A.height "2"
        ! A.fill "#0DB02B"
    centralCircle =
      S.circle
        ! A.fill "#E05206"
        ! A.cx "3.5"
        ! A.cy "3"
        ! A.r  "0.85"



{- |
Flag of the Nigeria

![flag of the Nigeria](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ng.svg)
-}
ng :: Svg
ng =
  flagV3Eq
    (2,1)
    "#008753"
    "#FFFFFF"
    "#008753"



{- |
Flag of the Netherlands

![flag of the Netherlands](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/nl.svg)
-}
nl :: S.Svg
nl =
  flagH3Eq
    (3,2)
    "#AE1C28"
    "#FFFFFF"
    "#21468B"



{- |
Flag of Norway

![flag of Norway](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/no.svg)
-}
no :: S.Svg
no =
    S.svg
      ! A.viewbox "0 0 22 16"
      ! A.width  "300"
      ! A.height "218.18"
      $ do
        background
        whiteCross
        blueCross
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 22)
        ! (A.height .: 16)
        ! A.stroke "none"
        ! A.fill "#BA0C2F"
    whiteCross =
      S.path
        ! A.fill "none"
        ! A.stroke "#FFFFFF"
        ! A.strokeWidth "4"
        ! A.d crossDirs
    blueCross =
      S.path
        ! A.fill "none"
        ! A.stroke "#00205B"
        ! A.strokeWidth "2"
        ! A.d crossDirs
    crossDirs = mkPath $ do
      m   8  0
      l   8 16
      m   0  8
      l  22  8



{- |
Flag of Polonia

![flag of Polonia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/pl.svg)
-}
pl :: Svg
pl =
    S.svg
      ! A.viewbox "0 0 8 5"
      ! A.width  "300"
      ! A.height "187.5"
      $ do
        topStripe
        botStripe
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 8)
        ! (A.height .: 2.5)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    botStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 2.5)
        ! (A.width  .: 8)
        ! (A.height .: 2.5)
        ! A.stroke "none"
        ! A.fill "#DC143C"



{- |
Flag of Portugal

![flag of Portugal](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/pt.svg)
-}
pt :: S.Svg
pt =
    S.svg
      ! A.viewbox "0 0 600 400"
      ! A.width  "300"
      ! A.height "200"
      $ do
        greenBand
        redBand
        ptCoA
  where
    greenBand =
      S.rect 
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 240)
        ! (A.height .: 400)
        ! A.stroke "none"
        ! A.fill "rgb(0,102,0)"
    redBand =
      S.rect
        ! (A.x .: 240)
        ! (A.y .:   0)
        ! (A.width  .: 360)
        ! (A.height .: 400)
        ! A.stroke "none"
        ! A.fill "rgb(255,0,0)"



{- |
Flag of Romania

![flag of Romania](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ro.svg)
-}
ro :: Svg
ro =
  flagV3Eq
    (3,2)
    "#002B7F"
    "#FCD116"
    "#CE1126"



{- |
Flag of Serbia

![flag of Serbia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/rs.svg)
-}
rs :: Svg
rs =
  flagH3Eq
    (3,2)
    "#C7363D"
    "#0C4077"
    "#FFFFFF"



{- |
Flag of Russia

![flag of Russia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ru.svg)
-}
ru :: S.Svg
ru =
  flagH3Eq
    (3,2)
    "#FFFFFF"
    "#0039A6"
    "#E4181C"



{- |
Flag of Sudan

![flag of Sudan](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/sd.svg)
-}
sd :: Svg
sd =
    S.svg
      ! A.viewbox "0 0 6 3"
      ! A.width  "300"
      ! A.height "150"
      $ do
        topBand
        midBand
        botBand
        leftTriangle
  where
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "6"
        ! A.height "1"
        ! A.fill "#D21034"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "1"
        ! A.width  "6"
        ! A.height "1"
        ! A.fill "#FFFFFF"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "2"
        ! A.width  "6"
        ! A.height "1"
        ! A.fill "#000000"
    leftTriangle =
      S.path
        ! A.fill "#007229"
        ! A.d triangleDirs
    triangleDirs = mkPath $ do
      m  0  0
      l  2  1.5
      l  0  3
      S.z



{- |
Flag of Sweden

![flag of Sweden](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/se.svg)
-}
se :: S.Svg
se =
    S.svg
      ! A.viewbox "0 0 16 10"
      ! A.width  "300"
      ! A.height "187.5"
      $ do
        background
        cross
  where
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 16)
        ! (A.height .: 10)
        ! A.stroke "none"
        ! A.fill "#006AA7"
    cross =
      S.path
        ! A.fill "none"
        ! A.stroke "#FECC02"
        ! A.strokeWidth "2"
        ! A.d crossDirs
    crossDirs = mkPath $ do
      m   6  0
      l   6 10
      m   0  5
      l  16  5



{- |
Flag of Slovenia

![flag of Slovenia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/si.svg)
-}
si :: Svg
si =
  S.svg
    ! A.viewbox "0 0 240 120"
    ! A.width  "300"
    ! A.height "150"
    $ do
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "240"
        ! A.height " 40"
        ! A.fill "#FFFFFF"
      S.rect
        ! A.x "0"
        ! A.y "40"
        ! A.width  "240"
        ! A.height " 40"
        ! A.fill "#0000FF"
      S.rect
        ! A.x "0"
        ! A.y "80"
        ! A.width  "240"
        ! A.height " 40"
        ! A.fill "#FF0000"
      siCoA




{- |
Flag of Slovakia

![flag of Slovakia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/sk.svg)
-}
sk :: Svg
sk =
    S.svg
      ! A.viewbox "0 0 900 600"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topStripe
        midStripe
        botStripe
        skCoA
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 900)
        ! (A.height .: 200)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    midStripe =
      S.rect
        ! (A.x .:   0)
        ! (A.y .: 200)
        ! (A.width  .: 900)
        ! (A.height .: 200)
        ! A.stroke "none"
        ! A.fill "#0B4EA2"
    botStripe =
      S.rect
        ! (A.x .:   0)
        ! (A.y .: 400)
        ! (A.width  .: 900)
        ! (A.height .: 200)
        ! A.stroke "none"
        ! A.fill "#EE1C25"



{- |
Flag of Sierra Leona

![flag of Sierra Leona](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/sl.svg)
-}
sl :: Svg
sl =
  flagH3Eq
    (3,2)
    "#1EB53A"
    "#FFFFFF"
    "#0072C6"



{- |
Flag of San Marino

![flag of San Marino](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/sm.svg)
-}
sm :: Svg
sm =
    S.svg
      ! A.viewbox "0 0 800 600"
      ! A.width  "300"
      ! A.height "225"
      $ do
        topStripe
        botStripe
        smCoA
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 800)
        ! (A.height .: 300)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    botStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 300)
        ! (A.width  .: 800)
        ! (A.height .: 300)
        ! A.stroke "none"
        ! A.fill "#5EB6E4"



{- |
Flag of Senegal

![flag of Senegal](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/sn.svg)
-}
sn :: Svg
sn =
    S.svg
      ! A.viewbox "0 0 300 200"
      ! A.width  "300"
      ! A.height "200"
      $ do
        leftBand
        midBand
        rightBand
        starRegular 5 33 (150,100)
          ! A.fill "#00853F"
  where
    leftBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "100"
        ! A.height "200"
        ! A.fill "#00853F"
    midBand =
      S.rect
        ! A.x "100"
        ! A.y "0"
        ! A.width  "100"
        ! A.height "200"
        ! A.fill "#FDEF42"
    rightBand =
      S.rect
        ! A.x "200"
        ! A.y "0"
        ! A.width  "100"
        ! A.height "200"
        ! A.fill "#E31B23"



{- |
Flag of Somalia

![flag of Somalia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/so.svg)
-}
so :: Svg
so =
    S.svg
      ! A.viewbox "0 0 300 200"
      ! A.width  "300"
      ! A.height "200"
      $ do
        back
        star
  where
    back =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "300"
        ! A.height "200"
        ! A.fill "#4189DD"
    r1 = 13/54 * 200
    star =
      starRegular 5 r1 (150,100)
        ! A.fill "#FFFFFF"



{- |
Flag of South Sudan

![flag of South Sudan](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ss.svg)
-}
ss :: Svg
ss =
    S.svg
      ! A.viewbox "0 0 114 57"
      ! A.width  "300"
      ! A.height "150"
      $ do
        whiteBackground
        topBand
        midBand
        botBand
        leftTriangle
        star
  where
    whiteBackground =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "114"
        ! A.height "57"
        ! A.fill "#FFFFFF"
    topBand =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "114"
        ! A.height "17"
        ! A.fill "#000000"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "20"
        ! A.width  "114"
        ! A.height "17"
        ! A.fill "#DB0A13"
    botBand =
      S.rect
        ! A.x "0"
        ! A.y "40"
        ! A.width  "114"
        ! A.height "17"
        ! A.fill "#018A2C"
    leftTriangle =
      S.path
        ! A.fill "#0645B1"
        ! A.d triangleDirs
    triangleDirs = mkPath $ do
      m    0    0
      l   (57 * cos(pi/6))   28.5
      l    0   57
      S.z
    (c1,c2) = (28.5 * tan(pi/6) , 28.5)
    star =
      starRegular 5 9.5 (c1,c2)
        ! A.fill "#FCDE02"
        ! A.transform (rotateAround (-90) c1 c2)



{- |
Flag of São Tomé and Príncipe

![flag of São Tomé and Príncipe](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/st.svg)
-}
st :: Svg
st =
    S.svg
      ! A.viewbox "0 0 28 14"
      ! A.width  "300"
      ! A.height "150"
      $ do
        back
        midBand
        leftTriangle
        leftStar
        rightStar
  where
    back =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "28"
        ! A.height "14"
        ! A.fill "#009739"
    midBand =
      S.rect
        ! A.x "0"
        ! A.y "4"
        ! A.width  "28"
        ! A.height "6"
        ! A.fill "#FFD100"
    leftTriangle =
      S.path
        ! A.fill "#EF3340"
        ! A.d triangleDirs
    triangleDirs = mkPath $ do
      m   0   0
      l   7   7
      l   0  14
      S.z
    leftStar =
      starRegular 5 2 (14,7)
        ! A.fill "#000000"
    rightStar =
      starRegular 5 2 (21,7)
        ! A.fill "#000000"


{- |
Flag of Chad

![flag of Chad](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/td.svg)
-}
td :: Svg
td =
  flagV3Eq
    (3,2)
    "#002669"
    "#FFCC00"
    "#D20F36"



{- |
Flag of Togo

![flag of Togo](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/tg.svg)
-}
tg :: Svg
tg =
    S.svg
      ! A.viewbox (S.toValue $ "0 0 " ++ show w ++ " " ++ show h)
      ! A.width  "300"
      ! A.height (S.toValue (300 / φ))
      $ do
        greenBackground
        topStripe
        botStripe
        redSquare
        star
  where
    φ = (1 + sqrt 5) / 2
    h = 500
    w = h * φ
    greenBackground =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! (A.width  .: w)
        ! (A.height .: h)
        ! A.fill "#006A4E"
    topStripe =
      S.rect
        ! A.x "0"
        ! A.y "100"
        ! (A.width  .: w)
        ! (A.height .: h/5)
        ! A.fill "#FFCE00"
    botStripe =
      S.rect
        ! A.x "0"
        ! A.y "300"
        ! (A.width  .: w)
        ! (A.height .: h/5)
        ! A.fill "#FFCE00"
    redSquare =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "300"
        ! A.height "300"
        ! A.fill "#D21034"
    star =
      starRegular 5 100 (150,150)
        ! A.fill "#FFFFFF"



{- |
Flag of Tunisia

![flag of Tunisia](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/tn.svg)
-}
tn :: Svg
tn =
    S.svg
      ! A.viewbox "0 0 60 40"
      ! A.width  "300"
      ! A.height "200"
      $ do
        redBackground
        whiteCircle
        moon
        star
  where
    redBackground =
      S.rect
        ! A.x "0"
        ! A.y "0"
        ! A.width  "60"
        ! A.height "40"
        ! A.fill "#E70013"
    whiteCircle =
      S.circle
        ! A.cx "30"
        ! A.cy "20"
        ! A.r  "10"
        ! A.fill "#FFFFFF"
    x1 = 31 + 81/16
    b = -40
    c = x1^2 - 64*x1 + 32^2 + 20^2 - 36
    y1 = 0.5 * (-b + sqrt (b^2 - 4*c))
    y2 = 0.5 * (-b - sqrt (b^2 - 4*c))
    moon =
      S.path
        ! A.fill "#E70013"
        ! A.d moonDirs
    moonDirs = mkPath $ do
      m   x1   y1
      aa  7.5  7.5  0  True  True  x1  y2
      aa  6    6    0  True  False x1  y1
      S.z
    star =
      starRegular 5 4.5 (32,20)
        ! A.fill "#E70013"
        ! A.transform (rotateAround (-90) 32 20)



{- |
Flag of Ukraine

![flag of Ukraine](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ua.svg)
-}
ua :: S.Svg
ua =
    S.svg
      ! A.viewbox "0 0 3 2"
      ! A.width  "300"
      ! A.height "200"
      $ do
        topStripe
        botStripe
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 3)
        ! (A.height .: 1)
        ! A.stroke "none"
        ! A.fill "#0057B7"
    botStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 1)
        ! (A.width  .: 3)
        ! (A.height .: 1)
        ! A.stroke "none"
        ! A.fill "#FFDD00"



{- |
Flag of the Uganda

![flag of the Uganda](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/ug.svg)
-}
ug :: Svg
ug =
    S.svg
      ! A.viewbox "0 0 900 600"
      ! A.width  "300"
      ! A.height "200"
      $ do
        mkRect "  0" "#000000"
        mkRect "100" "#FCDC04"
        mkRect "200" "#D90000"
        mkRect "300" "#000000"
        mkRect "400" "#FCDC04"
        mkRect "500" "#D90000"
  where
    mkRect ky color =
      S.rect
        ! A.x "0"
        ! A.y ky
        ! A.width  "900"
        ! A.height "100"
        ! A.fill color





{- |
Flag of the United Kingdom

![flag of the United Kingdom](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/uk.svg)
-}
uk :: S.Svg
uk =
    S.svg
      ! A.viewbox "0 0 50 30"
      ! A.width  "300"
      ! A.height "180"
      $ do
        scotland
        irelandBase
        irelandBase ! A.transform (rotateAround 180 mx my)
        englandRed
        englandWhite
  where
    w = 50
    h = 30
    mx = w / 2
    my = h / 2
    -- x0 = 3 / sin (atan (3/5))
    x1 = 2 / sin (atan (3/5))
    -- y0 = 3 / sin (atan (5/3))
    y1 = 2 / sin (atan (5/3))
    colWhite = "white"
    colBlue = "rgb(1,33,105)"
    colRed = "rgb(200,16,46)"
    scotland = do
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: w)
        ! (A.height .: h)
        ! A.stroke "none"
        ! A.fill colBlue
      S.path
        ! (A.strokeWidth .: 6)
        ! A.stroke colWhite
        ! A.fill "none"
        ! A.d scotlandDirs
    scotlandDirs = mkPath $ do
      m   0  0
      l   w  h
      m   0  h
      l   w  0
    irelandBase =
      S.path
        ! A.stroke "none"
        ! A.fill colRed
        ! A.d irelandDirs
    irelandDirs = mkPath $ do
      m   0         0
      l   0         y1
      l   (mx - x1) my
      l   mx        my
      S.z
      m   0         h
      l   x1        h
      l   mx        (my + y1)
      l   mx        my
      S.z
    englandRed =
      S.path 
        ! (A.strokeWidth .: 6)
        ! A.stroke colRed
        ! A.fill "none"
        ! A.d englandDirsRed
    englandWhite =
      S.path
        ! (A.strokeWidth .: 2)
        ! A.stroke colWhite
        ! A.fill "none"
        ! A.d englandDirsWhite
    englandDirsRed = mkPath $ do
      m   0   my
      l   w   my
      m   mx  0
      l   mx  h
    englandDirsWhite = mkPath $ do
      m   0         (my + 4)
      l   (mx - 4)  (my + 4)
      l   (mx - 4)  h
      m   (mx + 4)  h
      l   (mx + 4)  (my + 4)
      l   w         (my + 4)
      m   w         (my - 4)
      l   (mx + 4)  (my - 4)
      l   (mx + 4)  0
      m   (mx - 4)  0
      l   (mx - 4)  (my - 4)
      l   0         (my - 4)



{- |
Flag of the Holy See (Vatican City)

![flag of the Holy See (Vatican City)](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/va.svg)
-}
va :: S.Svg
va = 
    S.svg 
      ! A.viewbox "0 0 2500 2500"
      ! A.width  "300"
      ! A.height "300"
      $ do
        leftStripe
        rightStripe
        vaCoA
  where
    leftStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 1250)
        ! (A.height .: 2500)
        ! A.stroke "none"
        ! A.fill "#FFE000"
    rightStripe =
      S.rect
        ! (A.x .: 1250)
        ! (A.y .:    0)
        ! (A.width  .: 1250)
        ! (A.height .: 2500)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"



{- |
Flag of Kosovo

![flag of Kosovo](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/countryFlags/xk.svg)
-}
xk :: Svg
xk =
    S.svg
      ! A.viewbox "0 0 840 600"
      ! A.width  "300"
      ! A.height "214.285"
      $ do
        background
        xkCoA
        star (420 - d3, y3)
        star (420 - d2, y2)
        star (420 - d1, y1)
        star (420 + d1, y1)
        star (420 + d2, y2)
        star (420 + d3, y3)
  where
    d1 = 42
    d2 = 124.3
    d3 = 203
    y1 = 121.7
    y2 = 136
    y3 = 164.8
    background =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 840)
        ! (A.height .: 600)
        ! A.stroke "none"
        ! A.fill "#244AA5"
    star (c0,c1) =
      starRegular 5 36 (c0,c1)
        ! A.fill "#FFFFFF"