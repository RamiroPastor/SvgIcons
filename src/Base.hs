{-# LANGUAGE     OverloadedStrings       #-}


module Base where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A




evenOddSplit :: [a] -> ([a], [a])
evenOddSplit [] = ([], [])
evenOddSplit (x:xs) = (x:o, e)
  where (e,o) = evenOddSplit xs



addXmlns :: Svg -> Svg
addXmlns svg =
  svg
    ! customAttribute "xmlns" "http://www.w3.org/2000/svg"
    ! customAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"



infixl 5 .:
(.:) :: (AttributeValue -> Attribute ) -> Float -> Attribute
f .: x = f $ S.toValue x


distance :: (Float, Float) -> (Float, Float) -> Float
distance (ax,ay) (bx,by) =
  sqrt $ (bx - ax)**2 + (by - ay)**2


horizontalMirrorMatrix :: AttributeValue
horizontalMirrorMatrix =
  matrix (-1) 0 0 1 0 0

verticalMirrorMatrix :: AttributeValue
verticalMirrorMatrix =
  matrix 1 0 0 (-1) 0 0


-- frame takes the parameters of the viewbox
frame :: Float -> Float -> Float -> Float -> S.Svg
frame x y w h =
    S.path
      ! A.fill "none"
      ! A.stroke "black"
      ! A.strokeWidth "0.002"
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
