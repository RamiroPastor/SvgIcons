{-# LANGUAGE     OverloadedStrings       #-}



module Images.Flags where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Core.Geometry
import Core.Utils



flags :: [ (String , S.Svg) ]
flags =
  [ (,) "ad" ad
  , (,) "af" af
  , (,) "at" at
  , (,) "be" be
  , (,) "bg" bg
  , (,) "by" blr
  , (,) "ch" ch
  , (,) "cz" cz
  , (,) "de" de
  , (,) "dk" dk
  , (,) "ee" ee
  , (,) "es" es
  , (,) "fi" fi
  , (,) "fr" fr
  , (,) "gr" gr
  , (,) "ie" ie
  , (,) "is" is
  , (,) "it" it
  , (,) "lt" lt
  , (,) "lu" lu
  , (,) "lv" lv
  , (,) "mc" mc
  , (,) "md" md
  , (,) "mt" mt
  , (,) "nl" nl
  , (,) "no" no
  , (,) "pl" pl
  , (,) "pt" pt
  , (,) "ro" ro
  , (,) "ru" ru
  , (,) "se" se
  , (,) "sk" sk
  , (,) "ua" ua
  , (,) "uk" uk
  , (,) "va" va
  ]


flagV3Eq :: (Float,Float) -> String -> String -> String -> Svg
flagV3Eq (w,h) c1 c2 c3 =
    S.svg
      ! A.viewbox (S.toValue $ "0 0 " ++ show w ++ " " ++ show h)
      ! (A.width  .: 100*w)
      ! (A.height .: 100*h)
      $ S.g $ do
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


flagH3Eq :: (Float,Float) -> String -> String -> String -> Svg
flagH3Eq (w,h) c1 c2 c3 =
    S.svg
      ! A.viewbox (S.toValue $ "0 0 " ++ show w ++ " " ++ show h)
      ! (A.width  .: 100*w)
      ! (A.height .: 100*h)
      $ S.g $ do
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

-- flag of Andorra
ad :: Svg
ad = 
    S.svg
      ! A.viewbox "0 0 20 14"
      ! A.width  "200px"
      ! A.height "140px"
      $ S.g $ do
        leftStripe
        centreStripe
        rightStripe
  where
    leftStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 6.4)
        ! (A.height .: 14)
        ! A.stroke "none"
        ! A.fill "#10069F"
    centreStripe =
      S.rect
        ! (A.x .: 6.4)
        ! (A.y .: 0)
        ! (A.width  .: 7.2)
        ! (A.height .: 14)
        ! A.stroke "none"
        ! A.fill "#FEDD00"
    rightStripe =
      S.rect
        ! (A.x .: 13.6)
        ! (A.y .: 0)
        ! (A.width  .: 6.4)
        ! (A.height .: 14)
        ! A.stroke "none"
        ! A.fill "#D50032"


-- flag of Afghanistan
af :: Svg
af =
  flagV3Eq
    (3,2)
    "rgb(0,0,0)"
    "rgb(190,0,0)"
    "rgb(0,122,54)"


-- flag of Austria
at :: Svg
at = 
  flagH3Eq
    (3,2)
    "#C8102E"
    "#FFFFFF"
    "#C8102E"


-- flag of Belgium
be :: Svg
be =
  flagV3Eq
    (3,2.6)
    "#000000"
    "#FFE936"
    "#FF0F21"


-- flag of Bulgaria
bg :: Svg
bg =
  flagH3Eq
    (5,3)
    "#FFFFFF"
    "#00966E"
    "#D62612"


-- flag of Belarus
blr :: Svg
blr = 
    S.svg
      ! A.viewbox "0 0 90 45"
      ! A.width  "400px"
      ! A.height "200px"
      $ S.g $ do
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


-- flag of Switzerland
ch :: S.Svg
ch =
    S.svg
      ! A.viewbox "0 0 32 32"
      ! A.width  "200px"
      ! A.height "200px"
      $ S.g $ do
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


-- flag of Czech Republic
cz :: S.Svg
cz =
    S.svg
      ! A.viewbox "0 0 6 4"
      ! A.width  "300px"
      ! A.height "200px"
      $ S.g $ do
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


-- flag of Germany
de :: Svg
de =
  flagH3Eq
    (5,3)
    "rgb(0,0,0)"
    "rgb(255,0,0)"
    "rgb(255,204,0)"


-- flag of Denmark
dk :: Svg
dk =
    S.svg
      ! A.viewbox "0 0 37 28"
      ! A.width  "370px"
      ! A.height "280px"
      $ S.g $ do
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


-- flag of Estonia
ee :: Svg
ee =
  flagH3Eq
    (5.5, 3.5)
    "#0072CE"
    "#000000"
    "#FFFFFF"


-- flag of Spain
es :: Svg
es =
    S.svg
      ! A.viewbox "0 0 3 2"
      ! A.width  "300px"
      ! A.height "200px"
      $ S.g $ do
        redBandTop
        yellowBand
        redBandBot
  where
    colRed = "rgb(198,11,30)"
    colYellow = "rgb(255,196,0)"
    redBandTop =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 3)
        ! (A.height .: 0.5)
        ! A.stroke "none"
        ! A.fill colRed
    yellowBand =
      S.rect 
        ! (A.x .: 0)
        ! (A.y .: 0.5)
        ! (A.width  .: 3)
        ! (A.height .: 1)
        ! A.stroke "none"
        ! A.fill colYellow
    redBandBot =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 1.5)
        ! (A.width  .: 3)
        ! (A.height .: 0.5)
        ! A.stroke "none"
        ! A.fill colRed


-- flag of Finland
fi :: S.Svg
fi =
    S.svg
      ! A.viewbox "0 0 36 22"
      ! A.width  "360px"
      ! A.height "220px"
      $ S.g $ do
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


-- flag of France
fr :: S.Svg
fr =
  flagV3Eq
    (3,2)
    "rgb(0,85,164)"
    "rgb(255,255,255"
    "rgb(239,65,53)"


-- flag of Greece
gr :: Svg
gr =
    S.svg
      ! A.viewbox "0 0 27 18"
      ! A.width  "300px"
      ! A.height "200px"
      $ S.g $ do
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
    

-- flag of Ireland
ie :: S.Svg
ie =
  flagV3Eq
    (3,1.5)
    "rgb(22,155,98)"
    "rgb(255,255,255)"
    "rgb(255,136,62)"


-- flag of Iceland
is :: S.Svg
is = 
    S.svg 
      ! A.viewbox "0 0 25 18"
      ! A.width  "250px"
      ! A.height "180px"
      $ S.g $ do
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


-- flag of Italy
it :: S.Svg
it = 
  flagV3Eq
    (3,2)
    "rgb(0,140,69)"
    "rgb(244,249,255"
    "rgb(205,33,42)" 


-- flag of Lithuania
lt :: S.Svg
lt =
  flagH3Eq
    (5,3)
    "#FFB81C"
    "#046A38"
    "#BE3A34"


-- flag of Luxembourg
lu :: S.Svg
lu =
  flagH3Eq
    (3,2)
    "#EA141D"
    "#FFFFFF"
    "#51ADDA"


-- flag of Latvia
lv :: S.Svg
lv = 
    S.svg
      ! A.viewbox "0 0 20 10"
      ! A.width  "300px"
      ! A.height "150px"
      $ S.g $ do
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


-- flag of Monaco
mc :: S.Svg
mc =
    S.svg
      ! A.viewbox "0 0 5 4"
      ! A.width  "500px"
      ! A.height "400px"
      $ S.g $ do
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


-- flag of Moldova
md :: Svg
md =
  flagV3Eq
    (3,1.5)
    "#003DA5"
    "#FFD100"
    "#C8102E"


-- flag of Malta
mt :: S.Svg
mt =
    S.svg
      ! A.viewbox "0 0 3 2"
      ! A.width  "300px"
      ! A.height "200px"
      $ S.g $ do
        leftStripe
        rightStripe
  where
    leftStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 1.5)
        ! (A.height .: 2)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    rightStripe =
      S.rect
        ! (A.x .: 1.5)
        ! (A.y .: 0)
        ! (A.width  .: 1.5)
        ! (A.height .: 2)
        ! A.stroke "none"
        ! A.fill "#C01B22"


-- flag of Netherlands
nl :: S.Svg
nl =
  flagH3Eq
    (3,2)
    "#AE1C28"
    "#FFFFFF"
    "#21468B"


-- flag of Norway
no :: S.Svg
no =
    S.svg
      ! A.viewbox "0 0 22 16"
      ! A.width  "330px"
      ! A.height "240px"
      $ S.g $ do
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


--flag of Poland
pl :: Svg
pl =
    S.svg
      ! A.viewbox "0 0 8 5"
      ! A.width  "400px"
      ! A.height "250px"
      $ S.g $ do
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


-- flag of Portugal
pt :: S.Svg
pt =
    S.svg
      ! A.viewbox "0 0 3 2"
      ! A.width  "300px"
      ! A.height "200px"
      $ S.g $ do
        greenBand
        redBand
  where
    greenBand =
      S.rect 
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 6/5)
        ! (A.height .: 2)
        ! A.stroke "none"
        ! A.fill "rgb(0,102,0)"
    redBand =
      S.rect
        ! (A.x .: 6/5)
        ! (A.y .: 0)
        ! (A.width  .: 9/5)
        ! (A.height .: 2)
        ! A.stroke "none"
        ! A.fill "rgb(255,0,0)"


-- flag of Romania
ro :: Svg
ro =
  flagV3Eq
    (3,2)
    "#002B7F"
    "#FCD116"
    "#CE1126"


-- flag of Russia
ru :: S.Svg
ru =
  flagH3Eq
    (3,2)
    "#FFFFFF"
    "#0039A6"
    "#E4181C"


-- flag of Sweden
se :: S.Svg
se =
    S.svg
      ! A.viewbox "0 0 16 10"
      ! A.width  "320px"
      ! A.height "200px"
      $ S.g $ do
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


-- flag of Slovakia
sk :: Svg
sk =
    S.svg
      ! A.viewbox "0 0 18 12"
      ! A.width  "360px"
      ! A.height "240px"
      $ S.g $ do
        topStripe
        midStripe
        botStripe
  where
    topStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 18)
        ! (A.height .:  4)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"
    midStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 4)
        ! (A.width  .: 18)
        ! (A.height .:  4)
        ! A.stroke "none"
        ! A.fill "#0B4EA2"
    botStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 8)
        ! (A.width  .: 18)
        ! (A.height .:  4)
        ! A.stroke "none"
        ! A.fill "#EE1C25"
    



-- flag of Ukraine
ua :: S.Svg
ua =
    S.svg
      ! A.viewbox "0 0 3 2"
      ! A.width  "300px"
      ! A.height "200px"
      $ S.g $ do
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


-- flag of Great Britain
uk :: S.Svg
uk =
    S.svg
      ! A.viewbox "0 0 50 30"
      ! A.width  "250px"
      ! A.height "150px"
      $ S.g $ do
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


-- flag of the Holy See
va :: S.Svg
va = 
    S.svg 
      ! A.viewbox "0 0 1 1"
      ! A.width  "300px"
      ! A.height "300px"
      $ S.g $ do
        leftStripe
        rightStripe
  where
    leftStripe =
      S.rect
        ! (A.x .: 0)
        ! (A.y .: 0)
        ! (A.width  .: 0.5)
        ! (A.height .: 1)
        ! A.stroke "none"
        ! A.fill "#FFE000"
    rightStripe =
      S.rect
        ! (A.x .: 0.5)
        ! (A.y .: 0)
        ! (A.width  .: 0.5)
        ! (A.height .: 1)
        ! A.stroke "none"
        ! A.fill "#FFFFFF"