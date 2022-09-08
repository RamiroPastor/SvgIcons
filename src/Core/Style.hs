{-# LANGUAGE     OverloadedStrings       #-}



module Core.Style where

import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A





stdDims :: S.Svg -> S.Svg
stdDims content =
  S.svg content
    ! A.viewbox "-1 -1 2 2"
    ! A.height  "300px"
    ! A.width   "300px"


strkStyle :: S.Svg -> S.Svg
strkStyle svg =
  svg
    ! A.fill        "none"
    ! A.stroke      "black"
    ! A.strokeWidth "0.04"


fillStyle :: S.Svg -> S.Svg
fillStyle svg =
  svg
    ! A.fill        "black"
    ! A.stroke      "none"
    ! A.strokeWidth "0"


fullStyle :: S.Svg -> S.Svg
fullStyle svg =
  svg
    ! A.fill        "silver"
    ! A.stroke      "black"
    ! A.strokeWidth "0.03"



