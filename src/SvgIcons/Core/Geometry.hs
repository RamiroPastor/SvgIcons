{-# LANGUAGE     OverloadedStrings       #-}



{- |
Module for geometrical shapes.
-}

module SvgIcons.Core.Geometry 
  ( geometryExamples
  , regularPolygon
  , starPolygonFirstSpecies
  , starOutline
  , starFat
  , starRegular
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
>  [ (,) "regular_polygon_5" $ regularPolygon 5 0.9 (0,0)
>  , (,) "regular_polygon_6" $ regularPolygon 6 0.9 (0,0)
>  , (,) "star_polygon_5"    $ starPolygonFirstSpecies 5 0.9 (0,0)
>  , (,) "star_polygon_6"    $ starPolygonFirstSpecies 6 0.9 (0,0)
>  , (,) "star_fat_5"        $ starFat 5 0.9 (0,0)
>  , (,) "star_fat_6"        $ starFat 6 0.9 (0,0)
>  , (,) "star_regular_5"    $ starRegular 5 0.9 (0,0)
>  , (,) "star_regular_6"    $ starRegular 6 0.9 (0,0)
>  , (,) "asterisk_5"        $ asterisk 5 0.9 (0,0)
>  , (,) "asterisk_6"        $ asterisk 6 0.9 (0,0)
>  , (,) "asterisk_star_5"   $ asteriskStar 5 0.9 (0,0)
>  , (,) "asterisk_star_6"   $ asteriskStar 6 0.9 (0,0)
>  ]
-}
geometryExamples :: [ (String, Svg) ]
geometryExamples =
  [ (,) "regular_polygon_5" $ regularPolygon 5 0.9 (0,0)
  , (,) "regular_polygon_6" $ regularPolygon 6 0.9 (0,0)
  , (,) "star_polygon_5"    $ starPolygonFirstSpecies 5 0.9 (0,0)
  , (,) "star_polygon_6"    $ starPolygonFirstSpecies 6 0.9 (0,0)
  , (,) "star_fat_5"        $ starFat 5 0.9 (0,0)
  , (,) "star_fat_6"        $ starFat 6 0.9 (0,0)
  , (,) "star_regular_5"    $ starRegular 5 0.9 (0,0)
  , (,) "star_regular_6"    $ starRegular 6 0.9 (0,0)
  , (,) "asterisk_5"        $ asterisk 5 0.9 (0,0)
  , (,) "asterisk_6"        $ asterisk 6 0.9 (0,0)
  , (,) "asterisk_star_5"   $ asteriskStar 5 0.9 (0,0)
  , (,) "asterisk_star_6"   $ asteriskStar 6 0.9 (0,0)
  ]



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
`starOutline` builds a first species irregular star polygon.

The difference with the previous function is the stroke:
the previous function's stroke runs inside the figure 
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
    r2 = r1 * (2*cos(β/2) - 1/cos(β/2))


  
{- |
`asterisk` builds a regular asterisk.

Once again, it's a regular polygon but the stroke only joins
opposite vertices. To ensure that an asterisk is built, the Int
parameter gets multiplied by 2.

Examples:

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_6_strk.svg)
-}
asterisk
  :: Int             -- ^ half the number of vertices 
  -> Float           -- ^ circumradius
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
asterisk n r (c1,c2) =
    S.path
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

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_5_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_5_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_5_strk.svg)

![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_6_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_6_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/geometry/asterisk_star_6_strk.svg)
-}
asteriskStar
  :: Int             -- ^ half the number of vertices 
  -> Float           -- ^ circumradius
  -> (Float , Float) -- ^ coordinates of the central point
  -> Svg             -- ^ resulting svg path
asteriskStar n r1 (c1,c2) =
    S.path
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