{-# LANGUAGE     OverloadedStrings       #-}



module Mosaics where

import           Data.List (intersperse)
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



mosaicSample :: [ (String , S.Svg) ]
mosaicSample =
  [ (,) "nazariMosaic"  (nazariMosaic "orange" "purple")
  , (,) "lemonsMosaic"  (lemonsMosaic "gold")
  , (,) "squaresMosaic" (squaresMosaic "blue" "brown")
  -- , (,) "peopleMosaic"  peopleMosaic
  -- , (,) "hexMosaic1"    hexMosaic1
  ]



--------------------------------------------------------------------------------


nazariMosaic :: String -> String -> Svg
nazariMosaic colorUpper colorLower =
    svg
      ! A.viewbox (S.toValue vb)
      ! A.width  "300px"
      ! A.height (S.toValue $ (show $ 300*h) ++ "px") 
      $ do
        upperBirdie
        upperBirdie ! A.transform (translate    2     0 )
        upperBirdie ! A.transform (translate  (-1)    h )
        upperBirdie ! A.transform (translate    1     h )
        upperBirdie ! A.transform (translate  (-1)  (-h))
        lowerBirdie
        lowerBirdie ! A.transform (translate  (-2)    0 )
        lowerBirdie ! A.transform (translate  (-1)  (-h))
        lowerBirdie ! A.transform (translate    1   (-h))
        lowerBirdie ! A.transform (translate    1     h )
  where
    vb = "-1 " ++ show (-h) ++ " 2 " ++ show (2*h)
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


--------------------------------------------------------------------------------


lemonsMosaic :: String -> Svg
lemonsMosaic fillColor =
    svg
      ! A.viewbox "0.15 0 0.85 1"
      ! A.height "300px"
      ! A.width (S.toValue $ show (0.85 * 300) ++ "px")
      $ do
        -- lemon
        lemon ! A.transform (                             rotateAround   29  0.5 0.5)
        lemon ! A.transform (translate      0   (-0.5) <> rotateAround   29  0.5 0.5)
        lemon ! A.transform (translate      0     0.5  <> rotateAround   29  0.5 0.5)
        lemon ! A.transform (translate   0.43  (-0.25) <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate   0.43    0.25  <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate   0.43    0.75  <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate (-0.43) (-0.75) <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate (-0.43) (-0.25) <> rotateAround (-29) 0.5 0.5)
        lemon ! A.transform (translate (-0.43)   0.25  <> rotateAround (-29) 0.5 0.5)
  where
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


--------------------------------------------------------------------------------


squaresMosaic :: String -> String -> Svg
squaresMosaic c1 c2 =
    svg
      ! A.viewbox "-1 -1 2 2"
      ! A.width  "300px"
      ! A.height "300px"
      $ do
        corner
        corner ! A.transform                       (rotateAround 180 0   0  )
        corner ! A.transform (translate (-1)  0  <> rotateAround 270 0.5 0.5)
        corner ! A.transform (translate   0 (-1) <> rotateAround 90  0.5 0.5)
  where
    color1 = S.toValue c1
    color2 = S.toValue c2
    s = 0.07
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


--------------------------------------------------------------------------------


peopleMosaic :: Svg
peopleMosaic =
  S.svg
    ! A.viewbox "0 0 1 1"
    ! A.preserveaspectratio "none"
    $ do
      part1
      part2
  where
    part1 = S.g $ do
      mainLine
      mainLine ! A.transform (translate 0.5 0.5) 
    part2 = 
      part1 ! A.transform horizontalMirrorMatrix
    mainLine =
      S.path
        ! A.fill "none"
        ! A.stroke "lightgray"
        ! (A.strokeWidth .: 0.025)
        ! A.d mainPath
    mainPath = mkPath $ do
      m   0.5  0
      q   0.2  0.1  0.3  0.25
      q   0    0.3  0    0.5


--------------------------------------------------------------------------------


hexMosaic1 :: Svg
hexMosaic1 =
  S.svg
    ! A.viewbox (S.toValue $ concat $ intersperse " " $ map show [vbX, vbY, vbW, vbH])
    $ do
      -- frame vbX vbY vbW vbH
      S.defs baseHexDef
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
    baseHex = S.use ! A.xlinkHref "#hexMosaic1_baseHex"
    baseTile = 
      S.g $ do
        baseHex
        baseHex ! A.transform (rotateAround 120 0 0)
        baseHex ! A.transform (rotateAround 240 0 0)
    baseHexDef =
      S.path 
        ! A.id_ "hexMosaic1_baseHex"
        ! A.fill "none"
        ! A.stroke "silver"
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


--------------------------------------------------------------------------------
