{-# LANGUAGE     OverloadedStrings       #-}



module Core.Style 
  ( stdDims
  , fillStyle
  , fullStyle
  , strkStyle
  ) where

import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A




{- |
`stdDims` takes some svg content and wraps it with the
@\<svg\>@ tag, with attributes: 

* @viewbox="-1 -1 2 2"@
* @height="300px"@
* @width="300px"@ 
-}
stdDims :: S.Svg -> S.Svg
stdDims content =
  S.svg content
    ! A.viewbox "-1 -1 2 2"
    ! A.height  "100px"
    ! A.width   "100px"



{- |
Handy shortcut for the following attributes:

* @fill="none"@
* @stroke="black"@
* @stroke-width="0.04"@
-}
strkStyle :: S.Svg -> S.Svg
strkStyle svg =
  svg
    ! A.fill        "none"
    ! A.stroke      "black"
    ! A.strokeWidth "0.04"



{- |
Handy shortcut for the following attributes:

* @fill="black"@
* @stroke="none"@
* @stroke-width="0"@
-}
fillStyle :: S.Svg -> S.Svg
fillStyle svg =
  svg
    ! A.fill        "black"
    ! A.stroke      "none"
    ! A.strokeWidth "0"



{- |
Handy shortcut for the following attributes:

* @fill="silver"@
* @stroke="black"@
* @stroke-width="0.03"@
-}
fullStyle :: S.Svg -> S.Svg
fullStyle svg =
  svg
    ! A.fill        "silver"
    ! A.stroke      "black"
    ! A.strokeWidth "0.03"



