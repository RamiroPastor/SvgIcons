module Main (main) where

import Data.Default
import System.Directory

import Base
import Core
import Icons.File (svgFile)
import Icons.Human (svgHuman)
import Icons.Textarea (svgTextarea)
import Render



main :: IO ()
main = renderAllSvg "./svg"


renderAllSvg :: FilePath -> IO ()
renderAllSvg svgFolder = 
  do
    createDirectoryIfMissing False svgFolder
    removeDirectoryRecursive svgFolder
    createDirectory svgFolder
    createDirectory iconsFolder
    createDirectory filePath
    createDirectory humanPath
    createDirectory textareaPath
    createDirectory mosaicsFolder
    renderSvgFile filePath (map fillIcons svgFile)
    renderSvgFile filePath (map fullIcons svgFile)
    renderSvgFile filePath (map strkIcons svgFile)
    renderSvgFile humanPath (map fillIcons svgHuman)
    renderSvgFile humanPath (map fullIcons svgHuman)
    renderSvgFile humanPath (map strkIcons svgHuman)
    renderSvgFile textareaPath (map fillIcons svgTextarea)
    renderSvgFile textareaPath (map fullIcons svgTextarea)
    renderSvgFile textareaPath (map strkIcons svgTextarea)
    putStrLn "Svg files compiled correctly"
  where
    fillIcons (a,b) = (a ++ "_fill" , coreSvg def $ applyStyle fillStyle   b)
    fullIcons (a,b) = (a ++ "_full" , coreSvg def $ applyStyle fullStyle   b)
    strkIcons (a,b) = (a ++ "_strk" , coreSvg def $ applyStyle strkStyle b)
    -- test (a,b) = (a, coreSvg def $ b >> frame (-1) (-1) 2 2)
    iconsFolder  = svgFolder ++ "/icons"
    filePath     = svgFolder ++ "/icons/file/"
    humanPath    = svgFolder ++ "/icons/human/"
    textareaPath = svgFolder ++ "/icons/textarea/"
    mosaicsFolder = svgFolder ++ "/mosaics"
    