{-# LANGUAGE     OverloadedStrings       #-}



module SvgIcons.Core.Utils 
  ( evenOddSplit
  , addXmlns
  , (.:)
  , distance
  , horizontalMirrorMatrix
  , verticalMirrorMatrix
  , frame
  , cleanDecimals
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
`frame` is mainly used for testing purposes.

Takes the 4 numbers of the viewbox @(x0, y0, width, height)@
and returns a path which connects all 
consecutive corners of the viewbox and also connects opposite
middle points of the sides of the viewbox.

![framed svg](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/test/test_strk.svg)
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


-- Takes a number n and a string s, 
-- and returns a string equal to s except that every decimal
-- number inside s will have its decimal part capped at n digits
{- |
Please ignore this function.
-}
cleanDecimals :: Int -> String -> String
cleanDecimals n s = 
    f [] [] s
  where
    f _ acc [] = reverse acc
    f aux acc (c:cs) = 
      if c == '.'
        then f "." acc cs
        else if aux == []
          then f [] (c : acc) cs
          else if (not $ isDigit c)
            then f [] (c : aux ++ acc) cs
            else if (length aux < n)
              then f (c : aux) acc cs
              else f aux acc cs
