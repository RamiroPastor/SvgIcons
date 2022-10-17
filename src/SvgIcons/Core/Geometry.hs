{-# LANGUAGE     OverloadedStrings       #-}



{- |
Module for geometrical shapes.
-}

module SvgIcons.Core.Geometry 
  ( regularPolygon
  , starPolygonFirstSpecies
  , starOutline
  , starFat
  , starRegular
  , asterisk
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Utils



--------------------------------------------------------------------------------

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

__Returns a path__
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

__Returns a path__
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
        (c1 + sin (k * α))
        (c2 - cos (k * α))
      l 
        (c1 + sin (k * α + pi))
        (c2 - cos (k * α + pi))