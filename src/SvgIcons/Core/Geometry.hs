{-# LANGUAGE     OverloadedStrings       #-}



{- |
Module for geometrical shapes.

Tip: you may want to use @stroke-miterlimit@
-}

module SvgIcons.Core.Geometry 
  ( geometryExamples
  , anglesHelp
  , regularPolygon
  , starPolygonFirstSpecies
  , starPolygonWithBorder
  , starPolygonOverlap
  , starOutline
  , starFat
  , starRegular
  , starSlim
  , asterisk
  , asteriskStar
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils



--------------------------------------------------------------------------------

{- |
Some examples for this module.

>geometryExamples :: [ (String, Svg) ]
>geometryExamples =
>  [ (,) "regular_polygon_5"      $ regularPolygon 5 0.9 (0,0)
>  , (,) "regular_polygon_6"      $ regularPolygon 6 0.9 (0,0)
>  , (,) "star_polygon_5"         $ starPolygonFirstSpecies 5 0.9 (0,0)
>  , (,) "star_polygon_6"         $ starPolygonFirstSpecies 6 0.9 (0,0)
>  , (,) "star_polygon_border_5"  $ starPolygonWithBorder 5 0.9 0.1 (0,0)
>  , (,) "star_polygon_border_6"  $ starPolygonWithBorder 6 0.9 0.1 (0,0)
>  , (,) "star_polygon_overlap_5" $ starPolygonOverlap 5 0.9 0.1 (0,0)
>  , (,) "star_polygon_overlap_6" $ starPolygonOverlap 6 0.9 0.1 (0,0)
>  , (,) "star_fat_5"             $ starFat 5 0.9 (0,0)
>  , (,) "star_fat_6"             $ starFat 6 0.9 (0,0)
>  , (,) "star_regular_5"         $ starRegular 5 0.9 (0,0)
>  , (,) "star_regular_6"         $ starRegular 6 0.9 (0,0)
>  , (,) "star_slim_5"            $ starSlim 5 0.9 (0,0)
>  , (,) "star_slim_6"            $ starSlim 6 0.9 (0,0)
>  , (,) "asterisk_3"             $ asterisk 3 0.9 (0,0)
>  , (,) "asterisk_star_3"        $ asteriskStar 3 0.9 (0,0)
>  ]
-}
geometryExamples :: [ (String, Svg) ]
geometryExamples =
  [ (,) "regular_polygon_5"      $ regularPolygon 5 0.9 (0,0)
  , (,) "regular_polygon_6"      $ regularPolygon 6 0.9 (0,0)
  , (,) "star_polygon_5"         $ starPolygonFirstSpecies 5 0.9 (0,0)
  , (,) "star_polygon_6"         $ starPolygonFirstSpecies 6 0.9 (0,0)
  , (,) "star_polygon_border_5"  $ starPolygonWithBorder 5 0.9 0.1 (0,0)
  , (,) "star_polygon_border_6"  $ starPolygonWithBorder 6 0.9 0.1 (0,0)
  , (,) "star_polygon_overlap_5" $ starPolygonOverlap 5 0.9 0.1 (0,0)
  , (,) "star_polygon_overlap_6" $ starPolygonOverlap 6 0.9 0.1 (0,0)
  , (,) "star_fat_5"             $ starFat 5 0.9 (0,0)
  , (,) "star_fat_6"             $ starFat 6 0.9 (0,0)
  , (,) "star_regular_5"         $ starRegular 5 0.9 (0,0)
  , (,) "star_regular_6"         $ starRegular 6 0.9 (0,0)
  , (,) "star_slim_5"            $ starSlim 5 0.9 (0,0)
  , (,) "star_slim_6"            $ starSlim 6 0.9 (0,0)
  , (,) "asterisk_3"             $ asterisk 3 0.9 (0,0)
  , (,) "asterisk_star_3"        $ asteriskStar 3 0.9 (0,0)
  ]



{- |
`anglesHelp` is just a helpful graphic showing some angles (in radians)
involved in regular polygons and first species star polygons of @n@ vertices:

  (1) In black: central angle of a regular polygon.
  (2) In blue: inner angle of a regular polygon.
  (3) In red: outer angle of a first species star polygon.
  (4) In green: inner angle of a first species star polygon.

![angles help](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/anglesHelp.svg)
-}
anglesHelp :: Svg
anglesHelp =
    S.svg
      ! A.viewbox "-1 -1 2 2"
      ! A.width  "500"
      ! A.height "500"
      $ do
        pentagon
        centralAngle
        internalAngle
        starOuterAngle
        starInnerAngle
        mkText "apothema = r cos(π/n)" (0 , -0.2)
          ! A.fill "indigo"
  where
    r1 = 0.9
    vertice k =
      (r1 * sin(k*2*pi/7) , -r1 * cos(k*2*pi/7))
    pentagon = 
      S.path
        ! A.fill "white"
        ! A.stroke "silver"
        ! A.strokeWidth "0.02"
        ! A.d pentagonDirs
    pentagonDirs = mkPath $ do
      uncurry S.m $ vertice 0
      mapM_ ((uncurry S.l) . vertice . fromIntegral) [1 .. 6]
      S.z
    mkText txt (t1,t2) =
      S.text_ txt
        ! A.stroke "none"
        ! (A.x .: t1)
        ! (A.y .: t2)
        ! A.fontSize "0.09"
        ! A.fontWeight "bold"
        ! A.dominantBaseline "middle"
        ! A.textAnchor "middle"
    centralAngle =
      S.g
        ! A.stroke "black"
        ! A.fill "black"
        $ do
          S.path
            ! A.fill "none"
            ! A.strokeWidth "0.01"
            ! A.d centralDirs
          mkText "2π/n" (0 , 0.25)
    centralDirs = mkPath $ do
      uncurry S.m $ vertice 3
      uncurry S.l $ (0,0)
      uncurry S.l $ vertice 4
    internalAngle =
      S.g
        ! A.stroke "blue"
        ! A.fill "blue"
        $ do
          S.path
            ! A.fill "none"
            ! A.strokeWidth "0.01"
            ! A.d internalDirs
          mkText 
            "π - 2π/n"
            ( 0.18 + (fst $ vertice 6)
            , 0.05 + (snd $ vertice 6)
            )
    internalDirs = mkPath $ do
      uncurry S.m $ vertice 0
      uncurry S.l $ vertice 6
      uncurry S.l $ vertice 5  
    starOuterAngle =
      S.g
        ! A.stroke "red"
        ! A.fill "red"
        $ do
          S.path
            ! A.fill "none"
            ! A.strokeWidth "0.01"
            ! A.d starOuterAngleDirs
          mkText
            "π/n"
            ( 0.3  + (fst $ vertice 0)
            , 0.25 + (snd $ vertice 0)
            )
    starOuterAngleDirs = mkPath $ do
      uncurry S.m $ vertice 1
      uncurry S.l $ vertice 0
      uncurry S.l $ vertice 2
    starInnerAngle =
      S.g
        ! A.stroke "green"
        ! A.fill "green"
        $ do
          S.path
            ! A.fill "none"
            ! A.strokeWidth "0.01"
            ! A.d starInnerAngleDirs
          mkText
            "π - 4π/n"
            ( -0.05 + (fst $ vertice 3)
            , -0.5  + (snd $ vertice 3)
            )
    starInnerAngleDirs = mkPath $ do
      uncurry S.m $ vertice 1
      uncurry S.l $ vertice 3
      uncurry S.l $ vertice 5   




{- |
`regularPolygon` builds a regular polygon.

You can customize fill and stroke using the
usual [blaze-svg](https://hackage.haskell.org/package/blaze-svg) functions. For example:

>regularPolygon 5 100 (200,300)
>  ! A.fill "pink"
>  ! A.stroke "#0000FF"
>  ! A.strokeWidth "10"

will return a __path element__ corresponding to a 
regular pentagon of radius 100 centered at point
(200,300) filled in pink, green stroke and stroke
width 10.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/regular_polygon_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/regular_polygon_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/regular_polygon_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/regular_polygon_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/regular_polygon_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/regular_polygon_6_strk.svg)
-}
regularPolygon 
  :: Int             -- ^ number of vertices
  -> Float           -- ^ circumradius
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
regularPolygon n r (x0,y0) =
    S.path
      ! A.class_ "HaskellSvgIcons__regularPolygon"
      ! A.d directions
  where
    α  = 2 * pi / (fromIntegral n)
    draw k =
      l  (x0 + r * sin (k*α))
         (y0 - r * cos (k*α))
    directions =
      mkPath $ do
        m   x0   (y0 - r)
        mapM_ (draw . fromIntegral) [1..n]
        S.z



{- |
`starPolygonFirstSpecies` builds a first species regular star polygon.

First species means that one vertice is skipped when joining vertices.
The number of vertices must be strictly greater than 4.
Can be customized with the usual [blaze-svg](https://hackage.haskell.org/package/blaze-svg) functions.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_6_strk.svg)
-}
starPolygonFirstSpecies 
  :: Int             -- ^ number of vertices 
  -> Float           -- ^ circumradius
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
starPolygonFirstSpecies n r (c1,c2) =
    S.path
      ! A.class_ "HaskellSvgIcons__starPolygonFirstSpecies"
      ! A.d directions
  where
    α  = 2 * pi / (fromIntegral n)
    vertice k' = 
      let k = fromIntegral k'
      in 
        (,) (c1 + r * sin (k*α))
            (c2 - r * cos (k*α))
    verticesList = map vertice [0 .. (n-1)]
    directions =
      if even n 
        then 
          mkPath $ do
            m   (fst $ head verticesList)  (snd $ head verticesList)
            mapM_ (uncurry S.l) (fst $ evenOddSplit verticesList)
            S.z
            m   (fst $ verticesList !! 1)  (snd $ verticesList !! 1)
            mapM_ (uncurry S.l) (snd $ evenOddSplit verticesList)
            S.z
        else
          mkPath $ do
            m   (fst $ head verticesList)  (snd $ head verticesList)
            mapM_ (uncurry S.l) (tail $ fst $ evenOddSplit $ verticesList ++ verticesList)
            S.z


{- |
`starPolygonWithBorder` builds a first species regular star polygon with border.

First species means that one vertice is skipped when joining vertices.
The number of vertices must be strictly greater than 4.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_border_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_border_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_border_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_border_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_border_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_border_6_strk.svg)
-}
starPolygonWithBorder
  :: Int             -- ^ number of vertices 
  -> Float           -- ^ circumradius
  -> Float           -- ^ width of the line
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
starPolygonWithBorder n r1 w (c1,c2) =
    S.path
      ! A.class_ "HaskellSvgIcons__starPolygonWithBorder"
      ! A.d directions
  where
    β = 2 * pi / (fromIntegral n)
    ɣ = pi / 2 - β
    r2 = r1 - (w / tan ɣ)
    outerV k = (,)
      (c1 + r1 * sin (β * fromIntegral k))
      (c2 - r1 * cos (β * fromIntegral k))
    innerV k = (,)
      (c1 + r2 * sin (β * fromIntegral k))
      (c2 - r2 * cos (β * fromIntegral k))
    directions = 
      if even n
        then
          mkPath $ do
            (uncurry S.m)  (outerV 0)
            mapM_  (uncurry S.l) (map outerV [2, 4 .. n])
            S.z
            (uncurry S.m)  (outerV 1)
            mapM_  (uncurry S.l) (map outerV [3, 5 .. n])
            S.z
            (uncurry S.m)  (innerV 0)
            mapM_  (uncurry S.l) (reverse $ map innerV [2, 4 .. n])
            S.z
            (uncurry S.m)  (innerV 1)
            mapM_  (uncurry S.l) (reverse $ map innerV [3, 5 .. n])
            S.z
        else
          mkPath $ do
            (uncurry S.m)  (outerV 0)
            mapM_  (uncurry S.l) (map outerV [2, 4 .. (2*n-1)])
            S.z
            (uncurry S.m)  (innerV 0)
            mapM_  (uncurry S.l) (reverse $ map innerV [2, 4 .. (2*n-1)])
            S.z



{- |
`starPolygonOverlap` builds a first species regular star polygon with overlapping sides. 

Visually, it only difers from the previous function when both fill and stroke are enabled.

First species means that one vertice is skipped when joining vertices.
The number of vertices must be strictly greater than 4.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_overlap_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_overlap_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_overlap_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_overlap_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_overlap_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_polygon_overlap_6_strk.svg)

-}
starPolygonOverlap
  :: Int             -- ^ number of vertices 
  -> Float           -- ^ circumradius
  -> Float           -- ^ width of the line
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
starPolygonOverlap n r1 w (c1,c2) = 
    S.g 
      ! A.class_ "HaskellSvgIcons__starPolygonOverlap"
      $ do
        starPolygonWithBorder n r1 w (c1,c2)
          ! A.stroke "none"
        mapM_ (makeSide . fromIntegral . ((-1)*)) [0 .. (n-1)]
        fixFirstSide
  where
    β = 2 * pi / (fromIntegral n)
    r2 = r1 - w * tan β
    r3   = r1 * (2*cos(β/2) - 1/cos(β/2))
    apt3 = r3 * cos (β/2)
    r4   = r2 * (2*cos(β/2) - 1/cos(β/2))
    apt4 = r4 * cos (β/2)
    outerV k = (,)
      (c1 + r1 * sin (β * fromIntegral k))
      (c2 - r1 * cos (β * fromIntegral k))
    innerV k = (,)
      (c1 + r2 * sin (β * fromIntegral k))
      (c2 - r2 * cos (β * fromIntegral k))
    fixFirstSide = 
      let
        (om1,om2) = (c1 + apt3 * sin(-β) , c2 - apt3 * cos(-β))
        (im1,im2) = (c1 + apt4 * sin(-β) , c2 - apt4 * cos(-β))
        fillFix = mkPath $ do
          uncurry S.m $ outerV 0
          uncurry S.l $ (om1,om2)
          uncurry S.l $ (im1,im2)
          uncurry S.l $ innerV 0
          S.z
        strokeFix = mkPath $ do
          uncurry S.m $ outerV 0
          uncurry S.l $ (om1,om2)
          uncurry S.m $ (im1,im2)
          uncurry S.l $ innerV 0
      in
        S.g $ do
          S.path
            ! A.stroke "none"
            ! A.d fillFix
          S.path
            ! A.strokeLinecap "round"
            ! A.d strokeFix
    makeSide k =
      S.g $ do
        S.path
          ! A.d (sideDirs1 k)
          ! A.stroke "none"
        S.path
          ! A.d (sideDirs2 k)
          ! A.strokeLinecap "round"
    sideDirs1 k = mkPath $ do
      uncurry S.m $ outerV k
      uncurry S.l $ outerV (k-2)
      uncurry S.l $ innerV (k-2)
      uncurry S.l $ innerV k
      S.z
    sideDirs2 k = mkPath $ do
      uncurry S.m $ outerV k
      uncurry S.l $ outerV (k-2)
      uncurry S.m $ innerV (k-2)
      uncurry S.l $ innerV k




{-
starPolygonBorderOverlap
  :: Int             -- ^ number of vertices 
  -> Float           -- ^ circumradius
  -> Float           -- ^ width of the line
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
starPolygonBorderOverlap n r1 w (c1,c2) = do
    S.path
      ! A.d directions
  where
    β = 2 * pi / (fromIntegral n)
    ɣ = pi / 2 - β
    -- r2 = r1 - (w / tan ɣ)
    r2 = r1 - w * tan β
    -- r3 = r1 * (2*cos(β/2) - 1/cos(β/2))
    r3 = r1 * (cos β) / cos (β/2)
    -- r4 = r2 * (2*cos(β/2) - 1/cos(β/2))
    r4 = (r1 * cos β - w * sin β) / (cos (β/2))
    apt0 = r4 * cos (β/2)
    h0 = r2 - w - apt0
    y0 = h0 * sin ɣ
    r5 = sqrt $ y0^2 + (apt0 + w)^2
    θ = atan $ y0 / (apt0 + w)
    rMad = (r2 - w - apt0) / cos ɣ
    outerV k = (,)
      (c1 + r1 * sin (β * fromIntegral k))
      (c2 - r1 * cos (β * fromIntegral k))
    innerV k = (,)
      (c1 + r2 * sin (β * fromIntegral k))
      (c2 - r2 * cos (β * fromIntegral k))
    shortLegOuterV k = (,)
      (c1 + r3 * sin (β/2 + β * fromIntegral k))
      (c2 - r3 * cos (β/2 + β * fromIntegral k))
    longLegInnerV k = (,)
      (c1 + r4 * sin (-3*β/2 + β * fromIntegral k))
      (c2 - r4 * cos (-3*β/2 + β * fromIntegral k))
    -- shortLegInnerV k = (,)
    --   (c1 + r5 * sin (β * (fromIntegral k) + θ))
    --   (c2 - r5 * cos (β * (fromIntegral k) + θ))
    shortLegInnerV k = 
      let
        (i1,i2) = innerV k
      in
        (,)
          (i1 + rMad * sin (ɣ - β * fromIntegral k))
          (i2 + rMad * cos (ɣ - β * fromIntegral k))
    longLegOuterV k =
      let
        (i1,i2) = innerV (k-1)
      in
        (,)
          (i1 - rMad * sin (ɣ + β * fromIntegral (k-1)))
          (i2 + rMad * cos (ɣ + β * fromIntegral (k-1)))
    -- longLegOuterV k = (,)
    --   (c1 + r5 * sin (β * (fromIntegral $ k-1) - θ))
    --   (c2 - r5 * cos (β * (fromIntegral $ k-1) - θ))
    makeCorner k = do
      (uncurry S.m) (shortLegOuterV k)
      (uncurry S.l) (outerV         k)
      (uncurry S.l) (longLegOuterV  k)
      (uncurry S.l) (longLegInnerV  k)
      (uncurry S.l) (innerV         k)
      (uncurry S.l) (shortLegInnerV k)
      S.z
    directions = 
      mkPath $ mapM_ (makeCorner . fromIntegral . ((-1)*)) [0 .. (n-1)]
-}




{- |
`starOutline` builds a first species irregular star polygon.

The difference with function `starPolygonFirstSpecies` is the stroke:
that function's stroke runs inside the figure 
(so it would draw a pentagram), while this function's stroke
runs outside the shape (so it would draw a star).
There is no visual difference if you only fill the paths (with no stroke).


-}
starOutline 
  :: Int             -- ^ number of vertices
  -> Float           -- ^ circumradius
  -> Float           -- ^ inner radius (circumradius of the inner polygon)
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting path
starOutline n r1 r2 (c1,c2) =
    S.path
      ! A.class_ "HaskellSvgIcons__starOutline"
      ! A.d directions
  where
    β  = 2 * pi / (fromIntegral n)
    outerV k = (,)
      (c1 + r1 * sin (k*β))
      (c2 - r1 * cos (k*β))
    innerV k = (,)
      (c1 + r2 * sin (k*β + β/2))
      (c2 - r2 * cos (k*β + β/2))
    vertices = 
      foldr 
        (\k acc -> (outerV k) : (innerV k) : acc) 
        [] 
        (map fromIntegral [0 .. (n-1)])
    directions = mkPath $ do
      m     (fst $ head vertices) (snd $ head vertices)
      mapM_ (uncurry S.l) (tail vertices)
      S.z



{- |
`starFat` builds a first species irregular star polygon.

Works as `starOutline` but you don't need to specify
the inner radius, it is already coded so that you get a
"fat" star.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_fat_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_fat_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_fat_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_fat_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_fat_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_fat_6_strk.svg)
-}
starFat ::
  Int -> Float -> (Float , Float) -> Svg
starFat n r1 (c1,c2) =
    starOutline n r1 r2 (c1,c2)
  where
    β  = 2 * pi / (fromIntegral n)
    r2 = r1 * (1 - sin(β/2)*tan(β/2))



{- |
`starRegular` builds a first species regular star polygon.

Works as `starOutline` but you don't need to specify 
the inner radius, and you will get a regular star.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_regular_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_regular_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_regular_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_regular_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_regular_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_regular_6_strk.svg)
-}
starRegular ::
  Int -> Float -> (Float , Float) -> Svg
starRegular n r1 (c1,c2) =
    starOutline n r1 r2 (c1,c2)
  where
    β  = 2 * pi / (fromIntegral n)
    r2 = r1 * (2*cos(β/2) - 1/cos(β/2))  -- = r1 * (cos(β/2) - tan(β/2)*sin(β/2))



{- |
`starSlim` builds a first species irregular star polygon.

Works as `starOutline` but you don't need to specify
the inner radius, it is already coded so that you get a
"slim" star.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_slim_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_slim_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_slim_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_slim_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_slim_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/star_slim_6_strk.svg)
-}
starSlim :: 
  Int -> Float -> (Float, Float) -> Svg
starSlim n r1 (c1,c2) =
   starOutline n r1 r2 (c1,c2)
  where
    β  = 2 * pi / (fromIntegral n)
    r2 = r1 * cos β


  
{- |
`asterisk` builds a regular asterisk.

Once again, it's a regular polygon but the stroke only joins
opposite vertices. To ensure that an asterisk is built, the Int
parameter gets multiplied by 2.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_3_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_3_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_3_strk.svg)
-}
asterisk
  :: Int             -- ^ half the number of vertices 
  -> Float           -- ^ circumradius
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
asterisk n r (c1,c2) =
    S.path
      ! A.class_ "HaskellSvgIcons__asterisk"
      ! A.d directions
  where
    α  = pi / (fromIntegral n)
    directions = mkPath $ 
      mapM_ (joinOpposites . fromIntegral) [0 .. (n-1)]
    joinOpposites k = do
      m
        (c1 + r * sin (k * α))
        (c2 - r * cos (k * α))
      l 
        (c1 + r * sin (k * α + pi))
        (c2 - r * cos (k * α + pi))

    

{- |
`asteriskStar` builds a regular asterisk star.

It's a regular star but the stroke only joins
opposite vertices. To ensure that an asterisk is built, the Int
parameter gets multiplied by 2.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_3_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_3_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_3_strk.svg)
-}
asteriskStar
  :: Int             -- ^ half the number of vertices 
  -> Float           -- ^ circumradius
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
asteriskStar n r1 (c1,c2) =
    S.path
      ! A.class_ "HaskellSvgIcons__asteriskStar"
      ! A.d directions
  where
    α  = pi / (fromIntegral n)
    r2 = r1 * (2*cos(α/2) - 1/cos(α/2))
    directions = mkPath $ 
      mapM_ (joinOpposites . fromIntegral) [0 .. (n-1)]
    joinOpposites k = do
      m
        (c1 + r1 * sin (k * α))
        (c2 - r1 * cos (k * α))
      l 
        (c1 + r1 * sin (k * α + pi))
        (c2 - r1 * cos (k * α + pi))
      m 
        (c1 + r2 * sin (0.5 * α + k * α))
        (c2 - r2 * cos (0.5 * α + k * α))
      l
        (c1 + r2 * sin (0.5 * α + k * α + pi))
        (c2 - r2 * cos (0.5 * α + k * α + pi))