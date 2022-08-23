module Main (main) where

import Data.Default
import System.Directory

import Base
import Core
import Icons.Textarea
import Render



main :: IO ()
main = 
  do
    createDirectoryIfMissing False "./svg"
    removeDirectoryRecursive "./svg"
    createDirectory "./svg"
    createDirectory "./svg/icons"
    createDirectory textareaPath
    createDirectory "./svg/mosaics"
    renderSvgFile textareaPath (map fillIcons svgTextarea)
    renderSvgFile textareaPath (map fullIcons svgTextarea)
    renderSvgFile textareaPath (map strkIcons svgTextarea)
    putStrLn "Svg files compiled correctly"
  where
    fillIcons (a,b) = (a ++ "_fill" , coreSvg def $ applyStyle fillStyle   b)
    fullIcons (a,b) = (a ++ "_full" , coreSvg def $ applyStyle fullStyle   b)
    strkIcons (a,b) = (a ++ "_strk" , coreSvg def $ applyStyle strkStyle b)
    -- test (a,b) = (a, coreSvg def $ b >> frame (-1) (-1) 2 2)
    textareaPath = "./svg/icons/textarea/"
    