{-# LANGUAGE     OverloadedStrings       #-}



module Core where

import           Data.Default
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Base




data SvgStyle = SvgStyle
  { fillColor   :: S.AttributeValue
  , strokeColor :: S.AttributeValue
  , strokeSize  :: Float
  }

instance Default SvgStyle where
  def = SvgStyle 
    { fillColor   = "none"
    , strokeColor = "black"
    , strokeSize  = 0.04
    }

applyStyle :: SvgStyle -> S.Svg -> S.Svg
applyStyle svgStyle svg =
  svg
    ! A.fill   (fillColor svgStyle)
    ! A.stroke (strokeColor svgStyle)
    ! (A.strokeWidth .: strokeSize svgStyle)




data SvgDimensions = SvgDimensions
  { vBox :: S.AttributeValue
  , hDim :: S.AttributeValue
  , wDim :: S.AttributeValue
  }

instance Default SvgDimensions where
  def = SvgDimensions
    { vBox = "-1 -1 2 2"
    , hDim = "300px"
    , wDim = "300px"
    }



coreSvg :: SvgDimensions -> S.Svg -> S.Svg
coreSvg dims content =
  S.svg content
    ! A.viewbox (vBox dims)
    ! A.height  (hDim dims)
    ! A.width   (wDim dims)