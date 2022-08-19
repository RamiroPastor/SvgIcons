module Main (main) where

import Data.Default

import Base
import Core
import Icons.Textarea
import Render



main :: IO ()
main = 
  do
    renderSvgStandalone "./testRender/textarea/" (map f svgTextarea)
  where
    f (a,b) = (a, coreSvg def $ b >> frame (-1) (-1) 2 2)