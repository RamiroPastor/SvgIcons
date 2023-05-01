{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Core.Utils 
  ( evenOddSplit
  , addXmlns
  , (.:)
  , distance
  , horizontalMirrorMatrix
  , verticalMirrorMatrix
  , frame
  , rectangleWithRoundCorners
  ) where

import Data.Char
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A



{- |
Splits a list @xs@ into two lists:

* The first one contains all odd positioned elements of @xs@
* The second one contains all even positioned elements of @xs@
-}
evenOddSplit :: [a] -> ([a], [a])
evenOddSplit [] = ([], [])
evenOddSplit (x:xs) = (x:o, e)
  where (e,o) = evenOddSplit xs



{- |
Takes some `Svg` entity and adds two attributes:

* @xmlns="http://www.w3.org/2000/svg"@
* @xmlns:xlink="http://www.w3.org/1999/xlink"@
-}
addXmlns :: Svg -> Svg
addXmlns svg =
  svg
    ! customAttribute "xmlns" "http://www.w3.org/2000/svg"
    ! customAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"



{- |
Handy operator that converts a `Float` number
into an `AttributeValue` and feeds it to the `Attribute` function.
Example:

>S.path 
>  ! (A.strokeWidth .: 0.1) 
-}
infixl 5 .:
(.:) :: (AttributeValue -> Attribute ) -> Float -> Attribute
f .: x = f $ S.toValue x



{- |
Euclidian distance between two points.
-}
distance :: (Float, Float) -> (Float, Float) -> Float
distance (ax,ay) (bx,by) =
  sqrt $ (bx - ax)**2 + (by - ay)**2



{- |
Matrix for the horizontal symmetry __respect to the axis @x=0@__.
Use it with the transform `Attribute`:

>S.path
>  ! A.transform horizontalMirrorMatrix
-}
horizontalMirrorMatrix :: AttributeValue
horizontalMirrorMatrix =
  matrix (-1) 0 0 1 0 0



{- |
Matrix for the vertical symmetry __respect to the axis @y=0@__.
Use it with the transform `Attribute`:

>S.path
>  ! A.transform (verticalMirrorMatrix <> rotateAround 45 0 0)
-}
verticalMirrorMatrix :: AttributeValue
verticalMirrorMatrix =
  matrix 1 0 0 (-1) 0 0



{- |
`frame` is mainly used for testing purposes. It draws coordinate axis.

Takes the 4 numbers of the viewbox @(x0, y0, width, height)@
and returns a path which connects all 
consecutive corners of the viewbox and also connects opposite
middle points of the sides of the viewbox.

![framed svg](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/test/test.svg)
-}
frame 
  :: Float    -- ^ stroke-width
  -> String   -- ^ stroke color
  -> Float    -- ^ x0
  -> Float    -- ^ y0
  -> Float    -- ^ width
  -> Float    -- ^ height
  -> S.Svg    -- ^ resulting svg
frame s color x y w h =
    S.path
      ! A.fill "none"
      ! A.stroke (S.toValue color)
      ! (A.strokeWidth .: s)
      ! A.d frameDirs
  where
    frameDirs = mkPath $ do
      m   x       y
      l   x      (y + h)    
      l  (x + w) (y + h)
      l  (x + w)  y
      S.z
      m  (x + w/2)  y
      l  (x + w/2) (y + h)
      m   x        (y + h/2)
      l  (x + w)   (y + h/2)


{- |
Path of a rectangle with rounded corners.
-}
rectangleWithRoundCorners 
  :: Float              -- ^ corner radius
  -> (Float, Float)     -- ^ (semiwidth, semiheight)
  -> (Float, Float)     -- ^ central point (intersection of diagonals)
  -> S.Path             -- ^ resulting path
rectangleWithRoundCorners r0 (w0,h0) (px,py) =
  let
    x1 = px - w0
    x2 = px + w0
    y1 = py - h0
    y2 = py + h0
  in
    do
      m   (x1 + r0)  (y1     )
      aa   r0   r0   0   False  False  (x1     ) (y1 + r0)
      l   (x1     )  (y2 - r0)
      aa   r0   r0   0   False  False  (x1 + r0) (y2     )
      l   (x2 - r0)  (y2     )
      aa   r0   r0   0   False  False  (x2     ) (y2 - r0)
      l   (x2     )  (y1 + r0)
      aa   r0   r0   0   False  False  (x2 - r0) (y1     )
      S.z