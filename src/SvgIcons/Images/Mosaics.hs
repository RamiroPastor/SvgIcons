{-# LANGUAGE     OverloadedStrings       #-}



{- |
This module exports images that can be used as 
base tile to build a mosaic. This can be achieved 
using them with CSS as @background-image@ 
with @background-repeat: repeat@

They are already wrapped in the @\<svg\>@ tag, which means:

  * They are not composible.
  * They are ready for use, no wrapping work needed.
  * To build a perfectly matching mosaic, you must respect
  the width-to-height ratio (specific for every tile function), otherwise
  the resulting will have undesired gaps inbetween.

To fully grasp the potential, check this page: [https://ramiropastor.es/svg-icons](https://ramiropastor.es/svg-icons)
-}
module SvgIcons.Images.Mosaics 
  ( mosaicSample
  , nazariMosaic
  , triReligiousMosaic
  , hexagonsMosaic
  , beehiveMosaic
  , lemonsMosaic
  , arabicMosaic
  , cloudsMosaic
  , hexMosaic
  , arrowsMosaic
  , wiresMosaic
  , curvesMosaic
  , airplaneMosaic
  , octagonsMosaic
  , pentagonsMosaic
  , hexCirclesMosaic
  ) where

import           Data.List (intersperse)
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Geometry
import SvgIcons.Core.Utils


{- |
A list of all mosaics from this module, evaluated with
test colors. Please use more sensible colors in order to
avoid eye injuries on the viewer.

>mosaicSample :: [ (String , S.Svg) ]
>mosaicSample =
>  [ (,) "nazariMosaic"       (nazariMosaic "orange" "purple")
>  , (,) "triReligiousMosaic" (triReligiousMosaic "blue" "orange" "green")
>  , (,) "hexagonsMosaic"     (hexagonsMosaic "navy" "none")
>  , (,) "beehiveMosaic"       beehiveMosaic
>  , (,) "lemonsMosaic"       (lemonsMosaic "gold")
>  , (,) "arabicMosaic"       (arabicMosaic "blue" "brown")
>  , (,) "cloudsMosaic"       (cloudsMosaic "silver" "white")
>  , (,) "hexMosaic"          (hexMosaic    "limegreen")
>  , (,) "arrowsMosaic"       (arrowsMosaic "orange")
>  , (,) "wiresMosaic"        (wiresMosaic  "gray")
>  , (,) "curvesMosaic"        curvesMosaic
>  , (,) "airplaneMosaic"     (airplaneMosaic "deepskyblue")
>  , (,) "octagonsMosaic"     (octagonsMosaic "lightgray" "chocolate")
>  , (,) "pentagonsMosaic"    (pentagonsMosaic "deeppink" "black")
>  , (,) "hexCirclesMosaic"   (hexCirclesMosaic "gold")
>  ]
-}
mosaicSample :: [ (String , S.Svg) ]
mosaicSample =
  [ (,) "nazariMosaic"       (nazariMosaic "orange" "purple")
  , (,) "triReligiousMosaic" (triReligiousMosaic "blue" "orange" "green")
  , (,) "hexagonsMosaic"     (hexagonsMosaic "navy" "none")
  , (,) "beehiveMosaic"       beehiveMosaic
  , (,) "lemonsMosaic"       (lemonsMosaic "gold")
  , (,) "arabicMosaic"       (arabicMosaic "blue" "brown")
  , (,) "cloudsMosaic"       (cloudsMosaic "silver" "white")
  , (,) "hexMosaic"          (hexMosaic    "limegreen")
  , (,) "arrowsMosaic"       (arrowsMosaic "orange")
  , (,) "wiresMosaic"        (wiresMosaic  "gray")
  , (,) "curvesMosaic"        curvesMosaic
  , (,) "airplaneMosaic"     (airplaneMosaic "deepskyblue")
  , (,) "octagonsMosaic"     (octagonsMosaic "lightgray" "chocolate")
  , (,) "pentagonsMosaic"    (pentagonsMosaic "deeppink" "black")
  , (,) "hexCirclesMosaic"   (hexCirclesMosaic "gold")
  ]



--------------------------------------------------------------------------------

{- |
![nazari mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/nazariMosaic.svg)

Ratio between width and height is: \(h = \sqrt{3} \cdot w\)
-}
nazariMosaic :: String -> String -> Svg
nazariMosaic colorUpper colorLower =
    svg
      ! A.class_ "HaskellSvgIcons__nazariMosaic"
      ! A.viewbox (S.toValue vb)
      ! A.width  "300px"
      ! A.height (S.toValue $ (show $ 300*h) ++ "px") 
      $ do
        defs $ do
          upperBirdie ! A.id_ "HaskellSvgIcons-upperBirdie"
          lowerBirdie ! A.id_ "HaskellSvgIcons-lowerBirdie"
        use ! xlinkHref id1
        use ! xlinkHref id1 ! A.transform (translate    2     0 )
        use ! xlinkHref id1 ! A.transform (translate  (-1)    h )
        use ! xlinkHref id1 ! A.transform (translate    1     h )
        use ! xlinkHref id1 ! A.transform (translate  (-1)  (-h))
        use ! xlinkHref id2
        use ! xlinkHref id2 ! A.transform (translate  (-2)    0 )
        use ! xlinkHref id2 ! A.transform (translate  (-1)  (-h))
        use ! xlinkHref id2 ! A.transform (translate    1   (-h))
        use ! xlinkHref id2 ! A.transform (translate    1     h )
  where
    vb = "-1 " ++ show (-h) ++ " 2 " ++ show (2*h)
    id1 = "#HaskellSvgIcons-upperBirdie"
    id2 = "#HaskellSvgIcons-lowerBirdie"
    h   = sqrt 3
    apt = h / 3
    ax =  0
    ay = -h
    bx =  1
    by =  0
    cx = -1
    cy =  0
    dx =  0
    dy =  h
    mid x y = (x + y) / 2
    upperBirdie =
      S.path
        ! A.strokeWidth "0"
        ! A.fill (S.toValue colorUpper)
        ! A.d upperDirs
    lowerBirdie =
      S.path
        ! A.strokeWidth "0"
        ! A.fill (S.toValue colorLower)
        ! A.d lowerDirs
    upperDirs =
      mkPath $ do
        m   ax   ay
        aa  apt  apt  0  False  True    (mid ax bx)  (mid ay by)
        aa  apt  apt  0  False  False   bx           by
        aa  apt  apt  0  False  True    (mid bx cx)  (mid by cy)
        aa  apt  apt  0  False  False   cx           cy
        aa  apt  apt  0  False  True    (mid ax cx)  (mid ay cy)
        aa  apt  apt  0  False  False   ax           ay
        S.z
    lowerDirs =
      mkPath $ do
        m   bx   by
        aa  apt  apt  0  False  True    (mid bx dx)  (mid by dy)
        aa  apt  apt  0  False  False   dx           dy
        aa  apt  apt  0  False  True    (mid cx dx)  (mid cy dy)
        aa  apt  apt  0  False  False   cx           cy
        aa  apt  apt  0  False  True    (mid bx cx)  (mid by cy)
        aa  apt  apt  0  False  False   bx           by
        S.z



{- |
![tri-religious mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/triReligiousMosaic.svg)

Ratio between width and height is: \(h = \sqrt{3} \cdot w\)
-}
triReligiousMosaic :: String -> String -> String -> Svg
triReligiousMosaic fill1 fill2 fill3 =
    S.svg
      ! A.class_ "HaskellSvgIcons__triReligiousMosaic"
      ! A.viewbox (S.toValue $ "0 0 1 " ++ show (2*h))
      ! A.width  "300px"
      ! A.height (S.toValue $ (show $ 300 * sqrt 3) ++ "px")
      $ do
        defs $ do
          S.path
            ! A.strokeWidth "0"
            ! A.fill        "white"
            ! A.d           upperBird
            ! A.id_         "HaskellSvgIcons-triReligiousUpperBird"
          S.path
            ! A.strokeWidth "0"
            ! A.fill        (S.toValue fill1)
            ! A.d           lowerBird
            ! A.id_         "HaskellSvgIcons-triReligiousLowerBird"
          starPolygonFirstSpecies 6 0.17 (0.5 , h - apt)
            ! A.id_         "HaskellSvgIcons-triReligiousStar"
            ! A.transform   (rotateAround 30 0.5 (h - apt))
          S.path 
            ! A.strokeWidth "0"
            ! A.d           hexagonDirs
            ! A.id_         "HaskellSvgIcons-triReligiousHexagon"
        topBird
        topBird ! A.transform (translate  1       0   )
        topBird ! A.transform (translate  (-0.5)  h   )
        topBird ! A.transform (translate  0.5     h   )
        topBird ! A.transform (translate  (-0.5)  (-h))
        botBird
        botBird ! A.transform (translate  (-1)    0   )
        botBird ! A.transform (translate  (-0.5)  (-h))
        botBird ! A.transform (translate  0.5     (-h))
        botBird ! A.transform (translate  0.5     h   )
        hexagon ! A.fill "white"
        hexagon ! A.fill "white"           ! A.transform (translate  (-0.5)  (-h))
        hexagon ! A.fill "white"           ! A.transform (translate  0.5     (-h))
        star    ! A.fill (S.toValue fill2)
        star    ! A.fill (S.toValue fill3) ! A.transform (translate  (-0.5)  h   )
        star    ! A.fill (S.toValue fill3) ! A.transform (translate  0.5     h   )
  where
    topBird =
      S.use ! A.xlinkHref "#HaskellSvgIcons-triReligiousUpperBird"
    botBird =
      S.use ! A.xlinkHref "#HaskellSvgIcons-triReligiousLowerBird"
    hexagon =
      S.use ! A.xlinkHref "#HaskellSvgIcons-triReligiousHexagon"
    star =
      S.use ! A.xlinkHref "#HaskellSvgIcons-triReligiousStar"
    h   = (sqrt 3) / 2
    apt = h / 3
    (ax,ay) = (0.5 , 0  )
    (bx,by) = (1   , h  )
    (cx,cy) = (0   , h  )
    (dx,dy) = (0.5 , 2*h)
    mid x y = (x + y) / 2
    cos60 = 0.5
    sin60 = 0.5 * sqrt 3
    hexR  = 0.24
    (h1x,h1y) = (0.5 + hexR       , apt + h             )
    (h2x,h2y) = (0.5 + hexR*cos60 , apt + h - hexR*sin60)
    (h3x,h3y) = (0.5 - hexR*cos60 , apt + h - hexR*sin60)
    (h4x,h4y) = (0.5 - hexR       , apt + h             )
    (h5x,h5y) = (0.5 - hexR*cos60 , apt + h + hexR*sin60)
    (h6x,h6y) = (0.5 + hexR*cos60 , apt + h + hexR*sin60)
    hexagonDirs =
      mkPath $ do
        m  h1x  h1y
        l  h2x  h2y 
        l  h3x  h3y 
        l  h4x  h4y 
        l  h5x  h5y 
        l  h6x  h6y 
        S.z
    upperBird =
      mkPath $ do
        m   ax   ay
        aa  apt  apt  0  False  True    (mid ax bx)  (mid ay by)
        aa  apt  apt  0  False  False   bx           by
        aa  apt  apt  0  False  True    (mid bx cx)  (mid by cy)
        aa  apt  apt  0  False  False   cx           cy
        aa  apt  apt  0  False  True    (mid ax cx)  (mid ay cy)
        aa  apt  apt  0  False  False   ax           ay
    lowerBird =
      mkPath $ do
        m   bx   by
        aa  apt  apt  0  False  True    (mid bx dx)  (mid by dy)
        aa  apt  apt  0  False  False   dx           dy
        aa  apt  apt  0  False  True    (mid cx dx)  (mid cy dy)
        aa  apt  apt  0  False  False   cx           cy
        aa  apt  apt  0  False  True    (mid bx cx)  (mid by cy)
        aa  apt  apt  0  False  False   bx           by
        S.z



{- |
![hexagons mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/hexagonsMosaic.svg)

Ratio between width and height is: \(h = \sqrt{3} \cdot w\)
-}
hexagonsMosaic :: String -> String -> Svg
hexagonsMosaic strkColor fillColor = do
    S.svg
      ! A.class_ "HaskellSvgIcons__hexagonsMosaic"
      ! A.viewbox (S.toValue $ "0 0 1 " ++ show (3*rad))
      ! A.width  "300px"
      ! A.height (S.toValue $ (show $ 300 * sqrt 3) ++ "px")
      $ do
        S.path
          ! A.strokeLinecap "round"
          ! A.strokeWidth   "0.05"
          ! A.stroke        (S.toValue strkColor)
          ! A.fill          (S.toValue fillColor)
          ! A.d             hexagonDirs
  where
    apt = 0.5                        -- apotema
    rad = (2 / sqrt 3) * apt         -- radio
    aux = sqrt $ rad ^ 2 - apt ^ 2   -- proyeccion en el eje vertical
    hexagonDirs = mkPath $ do
      m   0.5  (  3 * rad + 0.05)   -- plus epsilon
      l   0.5  (2.5 * rad)
      m   0.5   0
      l   0.5  (0.5 * rad)
      l   0    (0.5 * rad + aux)
      l   0    (1.5 * rad + aux)
      l   0.5  (2.5 * rad)
      l   1    (1.5 * rad + aux)
      l   1    (0.5 * rad + aux)
      l   0.5  (0.5 * rad)



{- |
![beehive mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/beehiveMosaic.svg)

Ratio between width and height is: \(h = \sqrt{3} \cdot w\)
-}
beehiveMosaic :: Svg
beehiveMosaic = do
    S.svg
      ! A.class_ "HaskellSvgIcons__beehiveMosaic"
      ! A.viewbox (S.toValue $ "0 0 1 " ++ show (3*rad))
      ! A.width  "300px"
      ! A.height (S.toValue $ (show $ 300 * sqrt 3) ++ "px")
      $ do
        defs honeyGradient
        hexagon
  where
    apt = 0.5                        -- apotema
    rad = (2 / sqrt 3) * apt         -- radio
    aux = sqrt $ rad ^ 2 - apt ^ 2   -- proyeccion en el eje vertical
    hexagon =
      S.path
        ! A.strokeLinecap "round"
        ! A.strokeWidth   "0.05"
        ! A.stroke        "rgb(255,255,155)"
        ! A.fill          "url(#svg-honey-gradient)"
        ! A.d             hexagonDirs
    hexagonDirs =
      mkPath $ do
        m   0.5  (  3 * rad + 0.05)   -- plus epsilon
        l   0.5  (2.5 * rad)
        m   0.5  0
        l   0.5  (0.5 * rad)
        l   0    (0.5 * rad + aux)
        l   0    (1.5 * rad + aux)
        l   0.5  (2.5 * rad)
        l   1    (1.5 * rad + aux)
        l   1    (0.5 * rad + aux)
        l   0.5  (0.5 * rad)
    honeyGradient =
      radialgradient
        -- ! A.spreadmethod "reflect"
        ! A.cx "30%"
        ! A.cy "70%"
        ! A.r  "50%"
        ! A.fx "50%"
        ! A.fy "60%"
        ! A.id_ "svg-honey-gradient"
        $ do
          stop
            ! A.offset "0%"
            ! A.style  "stop-color: rgba(255,255,255,0.3)"
          stop
            ! A.offset "25%"
            ! A.style  "stop-color: rgba(255,255,0,0.4)"
          stop
            ! A.offset "50%"
            ! A.style  "stop-color: rgba(255,215,0,0.4)"
          stop
            ! A.offset "100%"
            ! A.style  "stop-color: rgba(255,140,0,0.4)"



{- |
![lemons mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/lemonsMosaic.svg)

Ratio between width and height is: \(w = 0.85 \cdot h\)
-}
lemonsMosaic :: String -> Svg
lemonsMosaic fillColor =
    svg
      ! A.class_ "HaskellSvgIcons__lemonsMosaic"
      ! A.viewbox "0.15 0 0.85 1"
      ! A.height "300px"
      ! A.width (S.toValue $ show (0.85 * 300) ++ "px")
      $ do
        defs $
          lemon ! A.id_ "HaskellSvgIcons-lemonTile"
        use ! xlinkHref iD ! A.transform (                             rotateAround   29  0.5 0.5)
        use ! xlinkHref iD ! A.transform (translate      0   (-0.5) <> rotateAround   29  0.5 0.5)
        use ! xlinkHref iD ! A.transform (translate      0     0.5  <> rotateAround   29  0.5 0.5)
        use ! xlinkHref iD ! A.transform (translate   0.43  (-0.25) <> rotateAround (-29) 0.5 0.5)
        use ! xlinkHref iD ! A.transform (translate   0.43    0.25  <> rotateAround (-29) 0.5 0.5)
        use ! xlinkHref iD ! A.transform (translate   0.43    0.75  <> rotateAround (-29) 0.5 0.5)
        use ! xlinkHref iD ! A.transform (translate (-0.43) (-0.75) <> rotateAround (-29) 0.5 0.5)
        use ! xlinkHref iD ! A.transform (translate (-0.43) (-0.25) <> rotateAround (-29) 0.5 0.5)
        use ! xlinkHref iD ! A.transform (translate (-0.43)   0.25  <> rotateAround (-29) 0.5 0.5)
  where
    iD = "#HaskellSvgIcons-lemonTile"
    r1 = 0.24
    r2 = r1
    k  = 0.2     -- k must be lower than r1
    x0 = 0.5
    y0 = 0.5
    y1 = y0 - k
    y2 = y0 + k
    f1 y = x0 - sqrt(r1^2 - (y - y0)^2)
    f2 y = x0 + sqrt(r1^2 - (y - y0)^2)
    lemon =
      S.path
        ! (A.strokeWidth .: 0)
        ! A.fill (S.toValue fillColor)
        ! A.d lemonDirs
    lemonDirs = mkPath $ do
      m   0.5      0.15
      aa  r2       r2     0  False  True   (f1 y1)  y1
      aa  r1       r1     0  False  False  (f1 y2)  y2  
      aa  r2       r2     0  False  True   0.5      0.85
      aa  r2       r2     0  False  True   (f2 y2)  y2
      aa  r1       r1     0  False  False  (f2 y1)  y1
      aa  r2       r2     0  False  True   0.5      0.15
      S.z



{- |
![arabic mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/arabicMosaic.svg)

Ratio between width and height is: \(h = w\)
-}
arabicMosaic :: String -> String -> Svg
arabicMosaic c1 c2 =
    svg
      ! A.class_ "HaskellSvgIcons__arabicMosaic"
      ! A.viewbox "-1 -1 2 2"
      ! A.width  "300px"
      ! A.height "300px"
      $ do
        defs $
          corner ! A.id_ "HaskellSvgIcons-arabicTile"
        S.g $ do
          use ! xlinkHref iD
          use ! xlinkHref iD ! A.transform                       (rotateAround 180 0   0  )
          use ! xlinkHref iD ! A.transform (translate (-1)  0  <> rotateAround 270 0.5 0.5)
          use ! xlinkHref iD ! A.transform (translate   0 (-1) <> rotateAround 90  0.5 0.5)
  where
    color1 = S.toValue c1
    color2 = S.toValue c2
    iD = "#HaskellSvgIcons-arabicTile"
    s = 0.05
    k1 = 0.16
    k2 = (1/3) * (0.5 - k1 + s)
    (ax, ay) = (0.5 + k1, 0.5 - k1)
    (bx, by) = (0.5 - k1, 0.5 + k1)
    corner =
      S.g 
        ! A.fill "none"
        ! (A.strokeWidth .: 2*s)
        ! A.strokeLinecap "round"
        $ do
          S.path ! A.stroke color1 ! A.d dirs1
          S.path ! A.stroke color2 ! A.d dirs2
          S.path ! A.stroke color1 ! A.d dirs3
          S.rect 
            ! (A.x .: ax - s)
            ! (A.y .: ay - s)
            ! (A.width  .: 2*s)
            ! (A.height .: 2*s)
            ! A.fill color1
    dirs1 = mkPath $ do
      m   0.5    1
      l   ax    (1-k2)
      l   ax     ay
    dirs2 = mkPath $ do
      m   1      0.5
      l  (1-k2)  by
      l   bx     by
      l   bx     k2
      l   0.5    0
    dirs3 = mkPath $ do
      m   ax     ay
      l   k2     ay
      l   0      0.5



{- |
![clouds mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/cloudsMosaic.svg)

Ratio between width and height is: \(h = w\)
-}
cloudsMosaic :: String -> String -> Svg
cloudsMosaic strkColor fillColor =
  S.svg
    ! A.class_ "HaskellSvgIcons__cloudsMosaic"
    ! A.viewbox "-1 -1 2 2"
    ! A.height "300"
    ! A.width  "300"
    $ S.g $ do
      part1
      part1 ! A.transform horizontalMirrorMatrix
  where
    part1 = S.g $ do
      mainLine
      mainLine ! A.transform (translate 1 1)
    mainLine =
      S.path
        ! A.fill (S.toValue fillColor)
        ! A.stroke (S.toValue strkColor)
        ! (A.strokeWidth .: 0.05)
        ! A.d mainPath
    mainPath = mkPath $ do
      m     0    (-1)
      q   (-0.6) (-0.8) (-0.4) (-0.5)
      q   (-1)   (-0.6) (-1)     0



{- |
![hex mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/hexMosaic.svg)

Ratio between width and height is: \(w = \sqrt{3} \cdot h\)
-}
hexMosaic :: String -> Svg
hexMosaic strkColor =
  S.svg
    ! A.class_ "HaskellSvgIcons__hexMosaic"
    ! A.viewbox (S.toValue $ concat $ intersperse " " $ map show [vbX, vbY, vbW, vbH])
    ! A.height "300px"
    ! A.width  (S.toValue $ show (300 * sqrt 3) ++ "px")
    $ do
      S.defs $ 
        baseHexDef
      S.g $ do
        baseHex  ! A.transform (translate 0 ((-3) * k))
        baseTile
        baseTile ! A.transform (translate ((-3) * k * cos30) ((-3) * k * sin30))
        baseTile ! A.transform (translate (  3  * k * cos30) ((-3) * k * sin30))
        baseTile ! A.transform (translate ((-3) * k * cos30) (  3  * k * sin30))
        baseTile ! A.transform (translate (  3  * k * cos30) (  3  * k * sin30))
  where
    vbX = (-1) * 0.5 * vbW
    vbY = (-1) * 0.5 * vbH
    vbW = 6 * k * cos30
    vbH = 3 * k
    k = 1  -- side of base hex
    cos30 = 0.5 * sqrt 3
    sin30 = 0.5
    baseHex = S.use ! A.xlinkHref "#HaskellSvgIcons-hexTile"
    baseTile = 
      S.g $ do
        baseHex
        baseHex ! A.transform (rotateAround 120 0 0)
        baseHex ! A.transform (rotateAround 240 0 0)
    baseHexDef =
      S.path 
        ! A.id_ "HaskellSvgIcons-hexTile"
        ! A.fill "none"
        ! A.stroke (S.toValue strkColor)
        ! A.strokeWidth "0.05"
        ! A.strokeLinecap "round"
        ! A.strokeLinejoin "round"
        ! A.d baseHexDirs
    baseHexDirs = S.mkPath $ do
      m   ( k * cos30) (k * sin30)
      l       0           0
      l       0           k
      lr  ( k * cos30) (k * sin30)
      m       0           0
      l   (-k * cos30) (k * sin30)
      lr      0           k
      lr  ( k * cos30) (k * sin30)
      ---
      m   (      k * cos30) (      k * sin30 + 1/3 * k)
      l   (1/3 * k * cos30) (1/3 * k * sin30 + 1/3 * k)
      l   (1/3 * k * cos30) (1/3 * k * sin30 + 2/3 * k)
      l   (      k * cos30) (      k * sin30 + 2/3 * k)
      ---
      m   (2/3 * k * cos30)        (2/3 * k * sin30 + 4/3 * k)
      lr  ( -1 * k * cos30)        ( -1 * k * sin30          )
      lr         0                 (2/3 * k * (-1)           )
      lr  (1/3 * k * cos30 * (-1)) (1/3 * k * sin30          )
      lr         0                 (2/3 * k                  )
      lr  (      k * cos30)        (      k * sin30)



{- |
![arrows mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/arrowsMosaic.svg)

Ratio between width and height is: \(h = \sqrt{3} \cdot w\)
-}
arrowsMosaic :: String -> Svg
arrowsMosaic strkColor =
    S.svg
      ! A.class_ "HaskellSvgIcons__arrowsMosaic"
      ! A.viewbox (S.toValue $ "0 0 1 " ++ show (2*h))
      ! A.width  "300px"
      ! A.height (S.toValue $ show (300 * sqrt 3) ++ "px")
      $ do
        defs $ 
          basePath ! A.id_ "HaskellSvgIcons-arrowTile"
        arrowTile
        arrowTile ! A.transform (translate  1       0   )
        arrowTile ! A.transform (translate  (-0.5)  h   )
        arrowTile ! A.transform (translate  0.5     h   )
        arrowTile ! A.transform (translate  (-0.5)  (-h))
        arrowTile ! A.transform (                           mirror)
        arrowTile ! A.transform (translate  (-1)    0    <> mirror)
        arrowTile ! A.transform (translate  (-0.5)  (-h) <> mirror)
        arrowTile ! A.transform (translate  0.5     (-h) <> mirror)
        arrowTile ! A.transform (translate  0.5     h    <> mirror)
  where
    h = sin60
    sin60  = 0.5 * sqrt 3
    cos60  = 0.5
    tan60  = sin60 / cos60
    tan60' = cos60 / sin60
    k = (1/6) * (cos60 / sin60)
    (ax,ay) = (tan60' * k       , h - k       )
    (bx,by) = (0.5 + 2*k*tan60' , h - k       )
    (cx,cy) = (0.5              , h - 3*k     )
    (dx,dy) = (0.5 - 2*k/sin60  , h - 3*k     )
    (ex,ey) = (0.5 +   k*tan60' , k           )
    (fx,fy) = (0.5 + 2*k*tan60' , h - 5*k     )
    (gx,gy) = (1   - 2*k*tan60' , h           )
    arrowTile =
      S.use ! A.xlinkHref "#HaskellSvgIcons-arrowTile"
    mirror = S.matrix 1 0 0 (-1) 0 (2*h)
    basePath = 
      S.path 
        ! d baseDirections
        ! fill "none"
        ! strokeWidth "0.03"
        ! stroke (S.toValue strkColor)
        ! strokeLinecap  "round"
        ! strokeLinejoin "round"
    baseDirections = mkPath $ do
      m   cx  cy
      l   bx  by
      l   ax  ay
      m   cx  cy
      l   dx  dy
      l   ex  ey
      m   cx  cy
      l   fx  fy
      l   gx  gy
  


{- |
![wires mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/wiresMosaic.svg)

Ratio between width and height is: \(h = w\)
-}
wiresMosaic :: String -> Svg
wiresMosaic strkColor =
  S.svg
    ! A.class_ "HaskellSvgIcons__wiresMosaic"
    ! A.viewbox "0 0 1 1"
    ! A.height "300px"
    ! A.width  "300px"
    $ do
      defs $
        topLeftCorner ! A.id_ "HaskellSvgIcons-wireTile"
      corner
      corner ! A.transform (matrix   1  0 0 (-1) 0 1)
      corner ! A.transform (matrix (-1) 0 0   1  1 0)
      corner ! A.transform (rotateAround 180 0.5 0.5)
  where
    oct k = (k * sqrt 2) / (2 + sqrt 2)
    corner =
      S.use ! A.xlinkHref "#HaskellSvgIcons-wireTile"
    topLeftCorner =
      S.path 
        ! A.fill "none"
        ! A.strokeWidth "0.002"
        ! A.stroke (S.toValue strkColor)
        ! A.d topLeftDirections
    topLeftDirections = mkPath $ do
      m   0.49  0
      l   0.49  0.5
      m   0.48  0
      l   0.48  0.5
      m   0.47  0
      l   0.47  0.5
      -----------------------
      m   0.435  0
      l   0.435  0.07
      l   0.42   0.12
      l   0.42   0.15
      m   0.42   0.12
      l   0.428  0.13
      l   0.428  0.15 
      m   0.435  0.07
      l   0.45   0.12
      l   0.45   0.22
      m   0.45   0.12
      l   0.44   0.15
      l   0.44   0.18
      -----------------------
      m   0.4   0
      l   0.4   0.15
      l   0.45  0.3
      l   0.45  0.5
      m   0.39   0
      l   0.39  0.15
      l   0.44  0.3
      l   0.44  0.5
      m   0.38  0
      l   0.38  0.15
      l   0.43  0.3
      l   0.43  0.5
      -----------------------
      m   0     0.025
      l   0.14  0.025
      l   0.18  0.045
      m   0     0.035
      l   0.14  0.035
      l   0.18  0.045
      m   0     0.045
      l   0.18  0.045
      m   0     0.055
      l   0.14  0.055
      l   0.18  0.045
      m   0     0.065
      l   0.14  0.065
      l   0.18  0.045
      -----------------------
      m   0     0.125
      l   0.14  0.125
      l   0.18  0.145
      m   0     0.135
      l   0.14  0.135
      l   0.18  0.145
      m   0     0.145
      l   0.18  0.145
      m   0     0.155
      l   0.14  0.155
      l   0.18  0.145
      m   0     0.165
      l   0.14  0.165
      l   0.18  0.145
      -----------------------
      m   0     0.225
      l   0.14  0.225
      l   0.18  0.245
      m   0     0.235
      l   0.14  0.235
      l   0.18  0.245
      m   0     0.245
      l   0.18  0.245
      m   0     0.255
      l   0.14  0.255
      l   0.18  0.245
      m   0     0.265
      l   0.14  0.265
      l   0.18  0.245
      -----------------------
      m   0.18  0.045
      l   0.22  0.045
      l   0.27  0.095
      l   0.3   0.095
      l   0.3   0.3
      l   0.4   0.35
      l   0.4   0.5
      m   0.18  0.145
      l   0.22  0.145
      l   0.25  0.175
      l   0.28  0.175
      l   0.28  0.3
      l   0.38  0.35
      l   0.38  0.5
      m   0.18  0.245
      l   0.22  0.245
      l   0.23  0.255
      l   0.26  0.255
      l   0.26  0.3
      l   0.36  0.35
      l   0.36  0.5
      -----------------------
      m   0.5    0
      l   0.35   0
      m   0.25   0
      l   0.25   0.05
      l   0.3    0.05
      m   0.35   0
      l   0.35   0.05
      l   0.3    0.05
      l   0.3    0.07
      l   0.35   0.07
      l   0.35   0.25   -- CENTER OF THE FLOWER
      l   0.33   0.23
      l   0.33   0.21
      m   0.33   0.23
      l   0.31   0.23
      m   0.35   0.25
      l   0.38   0.25
      m   0.35   0.25
      l   0.32   0.27
      l   0.32   0.29
      l   0.34   0.30
      m   0.35   0.25
      l   0.35   0.27
      l   0.34   0.28
      m   0.35   0.27
      l   0.36   0.28
      l   0.36   0.3
      m   0.35   0.25
      l   0.38   0.27
      l   0.38   0.29
      -- l   0.39   0.30
      -- l   0.37   0.30
      -- l   0.38   0.31
      l   0.415  0.31
      l   0.415  0.5
      m   0.35   0.25
      l   0.37   0.23
      l   0.37   0.21
      m   0.37   0.23
      l   0.39   0.23
      -----------------------
      m   0      0.49
      l   0.01   0.5
      m   0      0.47
      l   0.03   0.5
      m   0      0.45
      l   0.05   0.5
      m   0      0.43
      l   0.07   0.5
      -----------------------
      m   0           0.4
      l   (oct 0.1)   0.4
      l   0.1         (0.5 - oct 0.1)
      l   0.1         0.5
      m   0           0.38
      l   (oct 0.12)  0.38
      l   0.12        (0.5 - oct 0.12)
      l   0.12        0.5
      m   0           0.36
      l   (oct 0.14)  0.36
      l   0.14        (0.5 - oct 0.14)
      l   0.14        0.5
      m   0           0.34
      l   (oct 0.16)  0.34
      l   0.16        (0.5 - oct 0.16)
      l   0.16        0.5
      -----------------------
      m   0     0.3
      l   0.2   0.3
      l   0     0.5
      m   0.2   0.3
      l   0.2   0.5
      m   0.2   0.5
      l   0.415 0.5
      m   0.2   0.475
      l   0.36  0.475
      m   0.2   0.45
      l   0.36  0.45
      -----------------------
      m   0.28  0.45
      l   0.28  0.39
      l   0.25  0.39
      m   0.28  0.39
      l   0.31  0.39
      m   0.28  0.39
      l   0.28  0.36



{- |
![curves mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/curvesMosaic.svg)

Ratio between width and height is: \(h = w\)
-}
curvesMosaic :: Svg
curvesMosaic =
  S.svg
    ! A.class_ "HaskellSvgIcons__curvesMosaic"
    ! A.viewbox "0 0 1 1"
    ! A.height "300px"
    ! A.width  "300px"
    $ do
      curve1
      curve2
      curve3
      curve4
      curve5
      curve6
      curve7
      littleCircle
  where
    littleCircle = do
      S.circle ! A.fill "purple" ! A.r "0.03" ! A.cx "0" ! A.cy "0"
      S.circle ! A.fill "purple" ! A.r "0.03" ! A.cx "1" ! A.cy "0"
      S.circle ! A.fill "purple" ! A.r "0.03" ! A.cx "0" ! A.cy "1"
      S.circle ! A.fill "purple" ! A.r "0.03" ! A.cx "1" ! A.cy "1"
    curve1 = 
      S.path
        ! A.fill "none"
        ! A.stroke "orchid"
        ! A.strokeWidth "0.03"
        ! A.strokeLinecap "round"
        ! A.d curve1Dirs
    curve1Dirs = mkPath $ do
      m   0    0.25
      aa  0.2  0.2  0  True  True  0.3  0.7
      aa  0.2  0.2  0  True  True  0.7  0.7
      aa  0.2  0.2  0  True  True  1    0.25
    curve2 =
      S.path
        ! A.fill "none"
        ! A.stroke "crimson"
        ! A.strokeWidth "0.015"
        ! A.d curve2Dirs
    curve2Dirs = mkPath $ do
      m   0  0.25
      aa  0.125  0.125  0  True  True  1  0.5
      aa  0.125  0.125  0  True  True  0  0.75
      aa  0.125  0.125  0  True  True  1  1
    curve3 =
      S.path 
        ! A.fill "none"
        ! A.stroke "green"
        ! A.strokeWidth "0.015"
        ! A.strokeLinecap "round"
        ! A.d curve3Dirs
    curve3Dirs = mkPath $ do
      m   0    0
      aa  0.1  0.1  0  True  True  0  0.125
      m   1    0.125
      aa  0.1  0.1  0  True  False 1  0.25
      m   0    0.25
      aa  0.1  0.1  0  True  True  0  0.375
      m   1    0.375
      aa  0.1  0.1  0  True  False 1  0.5
      m   0    0.5
      aa  0.1  0.1  0  True  True  0  0.625
      m   1    0.625
      aa  0.1  0.1  0  True  False 1  0.75
      m   0    0.75
      aa  0.1  0.1  0  True  True  0  0.875
      m   1    0.875
      aa  0.1  0.1  0  True  False 1  1
      -- m   0     0
      -- l   0.16  0
      -- m   0.84  0
      -- l   1     0
    curve4 = 
      S.path 
        ! A.fill "none"
        ! A.stroke "gold"
        ! A.strokeWidth "0.02"
        ! A.strokeLinecap "round"
        ! A.d curve4Dirs
    curve4Dirs = mkPath $ do
      m   0.5  0
      c   0.1  0.1  0.8  0.1  0.5  0.25
      l   0.5  0.7
      c   0.9  0.8  0.1  0.9  0.5  0.9
      q   0.7  0.9  0.5  0.98
      l   0.5  1
    curve5 = 
      S.path 
        ! A.fill "none"
        ! A.stroke "teal"
        ! A.strokeWidth "0.025"
        ! A.strokeLinecap "round"
        ! A.d curve5Dirs
    curve5Dirs = mkPath $ do
      m   0.3  0
      aa  0.2  0.2  0  True  False  0.7  0
      m   0.3  1
      aa  0.2  0.2  0  True  True   0.7  1
    curve6 = 
      S.path 
        ! A.fill "deepskyblue"
        ! A.stroke "skyblue"
        ! A.strokeWidth "0.01"
        ! A.strokeLinecap "round"
        ! A.d curve6Dirs
    curve6Dirs = mkPath $ do
      m   0.47  0
      aa  0.03  0.03  0  True  False  0.53  0
      m   0.47  1
      aa  0.03  0.03  0  True  True   0.53  1
    curve7 = 
      S.path 
        ! A.fill "none"
        ! A.stroke "deeppink"
        ! A.strokeWidth "0.03"
        ! A.strokeLinecap "round"
        ! A.d curve7Dirs
    curve7Dirs = mkPath $ do
      m   0.25  0.8
      l   0.25  0.85
      m   0.75  0.8
      l   0.75  0.85
      m   0.25  0.1
      l   0.25  0.15
      m   0.75  0.1
      l   0.75  0.15



{- |
![airplane mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/airplaneMosaic.svg)

Ratio between width and height is: \(h =  w\)
-}
airplaneMosaic :: String -> Svg
airplaneMosaic fillColor =
  S.svg
    ! A.class_ "HaskellSvgIcons__airplaneMosaic"
    ! A.viewbox "0 0 2 2"
    ! A.height "300px"
    ! A.width  "300px"
    $ do
        defs $
          basicPlane ! A.id_ "HaskellSvgIcons-planeTile"
        plane ! A.transform (translate 1 (-0.5))
        plane ! A.transform (translate 0   0.5 )
        plane ! A.transform (translate 1   1.5 )
        plane ! A.transform (translate 0 (-0.5) <> S.matrix 1 0 0 (-1) 0 1)
        plane ! A.transform (translate 1   0.5  <> S.matrix 1 0 0 (-1) 0 1)
        plane ! A.transform (translate 0   1.5  <> S.matrix 1 0 0 (-1) 0 1)
  where
    r1 = 0.5 * sqrt 2
    y1 = 1.5 - 0.5 * sqrt 2
    plane =
      S.use ! A.xlinkHref "#HaskellSvgIcons-planeTile"
    basicPlane =
      S.path
        ! A.stroke "none"
        ! A.fill (S.toValue fillColor)
        ! A.d basicPlaneDirs
    basicPlaneDirs = mkPath $ do
      m   0         0.5
      l   (1 - r1)  0.5
      aa  r1  r1  0  False True  0.5  0
      aa  r1  r1  0  False True  r1   0.5
      l   1   0.5
      l   1   y1
      aa  r1  r1  0  False  False  0.5  1
      aa  r1  r1  0  False  False  0    y1
      S.z



{- |
![octagons mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/octagonsMosaic.svg)

Ratio between width and height is: \(h =  w\)
-}
octagonsMosaic :: String -> String -> Svg
octagonsMosaic fillColor strkColor =
    svg
      ! A.class_ "HaskellSvgIcons__octagonsMosaic"
      ! A.viewbox "0 0 4 4"
      ! A.height "300px"
      ! A.width  "300px"
      $ do
        S.path
          ! A.strokeWidth "0.2"
          ! A.stroke (S.toValue strkColor)
          ! A.fill   (S.toValue fillColor)
          ! A.d dirs
  where
    dirs = mkPath $ do
      m  1  1
      l  3  1
      l  3  3
      l  1  3
      S.z
      m  1  1
      l  0  0
      m  3  3
      l  4  4
      m  3  1
      l  4  0
      m  1  3
      l  0  4



{- |
![pentagons mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/pentagonsMosaic.svg)

Ratio between width and height is: \(w = \sqrt{3} \cdot h\)
-}
pentagonsMosaic :: String -> String -> Svg
pentagonsMosaic fillColor strkColor =
    svg
      ! A.class_ "HaskellSvgIcons__pentagonsMosaic"
      ! A.viewbox (S.toValue $ "0 0 " ++ (show x0) ++ " " ++ (show y0))
      ! A.height "300px"
      ! A.width  (S.toValue $ (show $ 300 * sqrt 3) ++ "px")
      $ do
        S.defs $
          S.path
            ! A.fill   (S.toValue fillColor)
            ! A.strokeWidth "0.15"
            ! A.stroke (S.toValue strkColor)
            ! A.strokeLinejoin "round"
            ! A.id_ "HaskellSvgIcons-pentagonTile"
            ! A.d pentagonDirs
        S.g $ do
          ---
          pentagon
          pentagon ! A.transform ((translate     h0  ( 5*l0/2)) <> S.rotate 120)
          pentagon ! A.transform ((translate     h0  ( 5*l0/2)) <> S.rotate  60)
          pentagon ! A.transform ((translate (  -h0) ( 9*l0/2)) <> S.rotate 240)
          pentagon ! A.transform ((translate (  -h0) ( 9*l0/2)) <> S.rotate 300)
          pentagon ! A.transform ((translate      0  ( 7*l0  )) <> S.rotate 180)
          --- 
          pentagon ! A.transform (                                 S.rotate 300)
          pentagon ! A.transform ((translate     h0  ( 5*l0/2)) <> S.rotate 180)
          pentagon ! A.transform ((translate     h0  ( 5*l0/2))                )
          pentagon ! A.transform ((translate ( 2*h0) ( 5*l0  )) <> S.rotate 120)
          pentagon ! A.transform ((translate ( 2*h0) ( 5*l0  )) <> S.rotate  60)
          pentagon ! A.transform ((translate      0  ( 7*l0  )) <> S.rotate 240)
          --- 
          pentagon ! A.transform ((translate ( 3*h0) (   l0/2)) <> S.rotate 120)
          pentagon ! A.transform ((translate ( 3*h0) (   l0/2)) <> S.rotate  60)
          pentagon ! A.transform ((translate     h0  ( 5*l0/2)) <> S.rotate 240)
          pentagon ! A.transform ((translate     h0  ( 5*l0/2)) <> S.rotate 300)
          pentagon ! A.transform ((translate ( 2*h0) ( 5*l0  )) <> S.rotate 180)
          pentagon ! A.transform ((translate ( 2*h0) ( 5*l0  ))                )
          pentagon ! A.transform ((translate ( 3*h0) (15*l0/2)) <> S.rotate 120)
          --- 
          pentagon ! A.transform ((translate ( 3*h0) (   l0/2)) <> S.rotate 180)
          pentagon ! A.transform ((translate ( 3*h0) (   l0/2))                )
          pentagon ! A.transform ((translate ( 4*h0) ( 3*l0  )) <> S.rotate 120)
          pentagon ! A.transform ((translate ( 4*h0) ( 3*l0  )) <> S.rotate  60)
          pentagon ! A.transform ((translate ( 2*h0) ( 5*l0  )) <> S.rotate 240)
          pentagon ! A.transform ((translate ( 2*h0) ( 5*l0  )) <> S.rotate 300)
          pentagon ! A.transform ((translate ( 3*h0) (15*l0/2)) <> S.rotate 180)
          --- 
          pentagon ! A.transform ((translate ( 3*h0) (   l0/2)) <> S.rotate 240)
          pentagon ! A.transform ((translate ( 3*h0) (   l0/2)) <> S.rotate 300)
          pentagon ! A.transform ((translate ( 4*h0) ( 3*l0  )) <> S.rotate 180)
          pentagon ! A.transform ((translate ( 4*h0) ( 3*l0  ))                )
          pentagon ! A.transform ((translate ( 5*h0) (11*l0/2)) <> S.rotate 120)
          pentagon ! A.transform ((translate ( 5*h0) (11*l0/2)) <> S.rotate  60)
          pentagon ! A.transform ((translate ( 3*h0) (15*l0/2)) <> S.rotate 240)
          --- 
          pentagon ! A.transform ((translate ( 6*h0) (   l0  )) <> S.rotate 120)
          pentagon ! A.transform ((translate ( 6*h0) (   l0  )) <> S.rotate  60)
          pentagon ! A.transform ((translate ( 4*h0) ( 3*l0  )) <> S.rotate 240)
          pentagon ! A.transform ((translate ( 4*h0) ( 3*l0  )) <> S.rotate 300)
          pentagon ! A.transform ((translate ( 5*h0) (11*l0/2)) <> S.rotate 180)
          pentagon ! A.transform ((translate ( 5*h0) (11*l0/2))                )
          --- 
          pentagon ! A.transform ((translate ( 6*h0) (   l0  )) <> S.rotate 180)
          pentagon ! A.transform ((translate ( 6*h0) (   l0  ))                )
          pentagon ! A.transform ((translate ( 7*h0) ( 7*l0/2)) <> S.rotate 120)
          pentagon ! A.transform ((translate ( 7*h0) ( 7*l0/2)) <> S.rotate  60)
          pentagon ! A.transform ((translate ( 5*h0) (11*l0/2)) <> S.rotate 240)
          pentagon ! A.transform ((translate ( 5*h0) (11*l0/2)) <> S.rotate 300)
          pentagon ! A.transform ((translate ( 6*h0) ( 8*l0  )) <> S.rotate 180)
          --- 
          pentagon ! A.transform ((translate ( 6*h0) (   l0  )) <> S.rotate 240)
          pentagon ! A.transform ((translate ( 6*h0) (   l0  )) <> S.rotate 300)
          pentagon ! A.transform ((translate ( 7*h0) ( 7*l0/2)) <> S.rotate 180)
          pentagon ! A.transform ((translate ( 7*h0) ( 7*l0/2))                )
          pentagon ! A.transform ((translate ( 8*h0) ( 6*l0  )) <> S.rotate 120)
          pentagon ! A.transform ((translate ( 8*h0) ( 6*l0  )) <> S.rotate  60)
          --- 
          pentagon ! A.transform ((translate ( 8*h0) (  -l0  ))                )
          pentagon ! A.transform ((translate ( 9*h0) ( 3*l0/2)) <> S.rotate 120)
          pentagon ! A.transform ((translate ( 9*h0) ( 3*l0/2)) <> S.rotate  60)
          pentagon ! A.transform ((translate ( 7*h0) ( 7*l0/2)) <> S.rotate 240)
          pentagon ! A.transform ((translate ( 7*h0) ( 7*l0/2)) <> S.rotate 300)
          pentagon ! A.transform ((translate ( 8*h0) ( 6*l0  )) <> S.rotate 180)
          pentagon ! A.transform ((translate ( 8*h0) ( 6*l0  ))                )
          --- 
          pentagon ! A.transform ((translate ( 9*h0) ( 3*l0/2)) <> S.rotate 180)
          pentagon ! A.transform ((translate ( 9*h0) ( 3*l0/2))                )
          pentagon ! A.transform ((translate (10*h0) ( 4*l0  )) <> S.rotate 120)
          pentagon ! A.transform ((translate (10*h0) ( 4*l0  )) <> S.rotate  60)
          pentagon ! A.transform ((translate ( 8*h0) ( 6*l0  )) <> S.rotate 240)
          pentagon ! A.transform ((translate ( 8*h0) ( 6*l0  )) <> S.rotate 300)
          ---
          pentagon ! A.transform ((translate (11*h0) (  -l0/2)) <> S.rotate  60)
          pentagon ! A.transform ((translate ( 9*h0) ( 3*l0/2)) <> S.rotate 240)
          pentagon ! A.transform ((translate ( 9*h0) ( 3*l0/2)) <> S.rotate 300)
          pentagon ! A.transform ((translate (10*h0) ( 4*l0  )) <> S.rotate 180)
          pentagon ! A.transform ((translate (10*h0) ( 4*l0  ))                )
          pentagon ! A.transform ((translate (11*h0) (13*l0/2)) <> S.rotate 120)
          pentagon ! A.transform ((translate (11*h0) (13*l0/2)) <> S.rotate  60)
          ---
          pentagon ! A.transform ((translate (11*h0) (  -l0/2))                )
          pentagon ! A.transform ((translate (12*h0) ( 2*l0  )) <> S.rotate 120)
          pentagon ! A.transform ((translate (12*h0) ( 2*l0  )) <> S.rotate  60)
          pentagon ! A.transform ((translate (10*h0) ( 4*l0  )) <> S.rotate 240)
          pentagon ! A.transform ((translate (10*h0) ( 4*l0  )) <> S.rotate 300)
          pentagon ! A.transform ((translate (11*h0) (13*l0/2)) <> S.rotate 180)
          pentagon ! A.transform ((translate (11*h0) (13*l0/2))                )
          ---
          pentagon ! A.transform ((translate (11*h0) (  -l0/2)) <> S.rotate 300)
          pentagon ! A.transform ((translate (12*h0) ( 2*l0  )) <> S.rotate 180)
          pentagon ! A.transform ((translate (12*h0) ( 2*l0  ))                )
          pentagon ! A.transform ((translate (13*h0) ( 9*l0/2)) <> S.rotate 120)
          pentagon ! A.transform ((translate (13*h0) ( 9*l0/2)) <> S.rotate  60)
          pentagon ! A.transform ((translate (11*h0) (13*l0/2)) <> S.rotate 240)
          pentagon ! A.transform ((translate (11*h0) (13*l0/2)) <> S.rotate 300)
          ---
          pentagon ! A.transform ((translate (14*h0) ( 0*l0  )) <> S.rotate  60)
          pentagon ! A.transform ((translate (12*h0) ( 2*l0  )) <> S.rotate 240)
          pentagon ! A.transform ((translate (12*h0) ( 2*l0  )) <> S.rotate 300)
          pentagon ! A.transform ((translate (13*h0) ( 9*l0/2)) <> S.rotate 180)
          pentagon ! A.transform ((translate (13*h0) ( 9*l0/2))                )
          pentagon ! A.transform ((translate (14*h0) ( 7*l0  )) <> S.rotate 120)
          ---
          pentagon ! A.transform ((translate (14*h0) ( 0*l0  ))                )
          pentagon ! A.transform ((translate (15*h0) ( 5*l0/2)) <> S.rotate 120)
          pentagon ! A.transform ((translate (15*h0) ( 5*l0/2)) <> S.rotate  60)
          pentagon ! A.transform ((translate (13*h0) ( 9*l0/2)) <> S.rotate 240)
          pentagon ! A.transform ((translate (13*h0) ( 9*l0/2)) <> S.rotate 300)
          pentagon ! A.transform ((translate (14*h0) ( 7*l0  )) <> S.rotate 180)
  where
    pentagon = 
      S.use ! A.xlinkHref "#HaskellSvgIcons-pentagonTile"
    r0 = 1
    a0 = r0 / 2
    l0 = r0 * sqrt 3
    h0 = r0 + a0
    y0 = l0 * 7
    x0 = y0 * sqrt 3
    pentagonDirs = mkPath $ do
      m     0     0
      lr (-r0)   l0
      lr   a0   (l0/2)
      lr   r0     0
      lr   a0  (-l0/2)
      lr (-r0) (-l0)
      S.z
      


{- |
![hexCircles mosaic](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/images/mosaics/hexCirclesMosaic.svg)

Ratio between width and height is: \(h = \sqrt{3} \cdot w\)
-}
hexCirclesMosaic :: String -> Svg
hexCirclesMosaic strkColor = do
    S.svg
      ! A.class_ "HaskellSvgIcons__hexCirclesMosaic"
      ! A.viewbox (S.toValue $ "0 0 1 " ++ show (3*rad))
      ! A.width  "300px"
      ! A.height (S.toValue $ (show $ 300 * sqrt 3) ++ "px")
      $ S.g
        ! A.fill "none"
        ! A.strokeWidth "0.033"
        ! A.stroke (S.toValue strkColor)
        $ do
          circ ! (A.cx .: 0.5) ! (A.cy .: rad + aux  )
          --
          circ ! (A.cx .: 0.5) ! (A.cy .: aux        )
          circ ! (A.cx .: 0  ) ! (A.cy .: rad        )
          circ ! (A.cx .: 0  ) ! (A.cy .: rad + 2*aux)
          circ ! (A.cx .: 0.5) ! (A.cy .: aux + 2*rad)
          circ ! (A.cx .: 1  ) ! (A.cy .: rad + 2*aux)
          circ ! (A.cx .: 1  ) ! (A.cy .: rad        )
          --
          circ ! (A.cx .: 0      ) ! (A.cy .: 0        )
          circ ! (A.cx .: 0      ) ! (A.cy .: 3*rad    )
          circ ! (A.cx .: 1      ) ! (A.cy .: 3*rad    )
          circ ! (A.cx .: 1      ) ! (A.cy .: 0        )
          circ ! (A.cx .: 0 - apt) ! (A.cy .: rad + aux)
          circ ! (A.cx .: 1 + apt) ! (A.cy .: rad + aux)
          -- 
          circ ! (A.cx .: 0.5    ) ! (A.cy .:     0 - aux)
          circ ! (A.cx .: 0.5    ) ! (A.cy .: 3*rad + aux)
          circ ! (A.cx .: 0 - apt) ! (A.cy .: aux        )
          circ ! (A.cx .: 1 + apt) ! (A.cy .: aux        )
          circ ! (A.cx .: 0 - apt) ! (A.cy .: aux + 2*rad)
          circ ! (A.cx .: 1 + apt) ! (A.cy .: aux + 2*rad)
  where
    apt = 0.5                        -- apotema
    rad = (2 / sqrt 3) * apt         -- radio
    aux = sqrt $ rad ^ 2 - apt ^ 2
    circ = S.circle ! (A.r .: rad)
