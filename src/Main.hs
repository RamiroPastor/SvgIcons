module Main (main) where

import Data.Default
import System.Directory

import Base
import Core
import Icons.Business (svgBusiness)
import Icons.File     (svgFile)
import Icons.Human    (svgHuman)
import Icons.Office   (svgOffice)
import Icons.Textarea (svgTextarea)
import Icons.Tools    (svgTools)
import Mosaics (mosaicSample)
import Render





main :: IO ()
main = renderAll "./svg"



renderAll :: FilePath -> IO ()
renderAll svgFolder = do
  createDirectoryIfMissing False svgFolder
  removeDirectoryRecursive       svgFolder
  createDirectory                svgFolder
  renderIcons   (svgFolder ++ "/icons/")
  renderMosaics (svgFolder ++ "/mosaics/")
  putStrLn "Svg files compiled correctly"



renderIcons :: FilePath -> IO ()
renderIcons path = 
  do
    createDirectory path
    createDirectory businessPath
    createDirectory filePath
    createDirectory humanPath
    createDirectory officePath
    createDirectory textareaPath
    createDirectory toolsPath
    renderSvgFiles businessPath (map fillIcons svgBusiness)
    renderSvgFiles businessPath (map fullIcons svgBusiness)
    renderSvgFiles businessPath (map strkIcons svgBusiness)
    renderSvgFiles filePath (map fillIcons svgFile)
    renderSvgFiles filePath (map fullIcons svgFile)
    renderSvgFiles filePath (map strkIcons svgFile)
    renderSvgFiles humanPath (map fillIcons svgHuman)
    renderSvgFiles humanPath (map fullIcons svgHuman)
    renderSvgFiles humanPath (map strkIcons svgHuman)
    renderSvgFiles officePath (map fillIcons svgOffice)
    renderSvgFiles officePath (map fullIcons svgOffice)
    renderSvgFiles officePath (map strkIcons svgOffice)
    renderSvgFiles textareaPath (map fillIcons svgTextarea)
    renderSvgFiles textareaPath (map fullIcons svgTextarea)
    renderSvgFiles textareaPath (map strkIcons svgTextarea)
    renderSvgFiles toolsPath (map fillIcons svgTools)
    renderSvgFiles toolsPath (map fullIcons svgTools)
    renderSvgFiles toolsPath (map strkIcons svgTools)
  where
    fillIcons (a,b) = (a ++ "_fill" , coreSvg def $ applyStyle fillStyle b)
    fullIcons (a,b) = (a ++ "_full" , coreSvg def $ applyStyle fullStyle b)
    strkIcons (a,b) = (a ++ "_strk" , coreSvg def $ applyStyle strkStyle b)
    -- test (a,b) = (a, coreSvg def $ b >> frame (-1) (-1) 2 2)
    businessPath = path ++ "business/"
    filePath     = path ++ "file/"
    humanPath    = path ++ "human/"
    officePath   = path ++ "office/"
    textareaPath = path ++ "textarea/"
    toolsPath    = path ++ "tools/"
    


renderMosaics :: FilePath -> IO ()
renderMosaics path = do
    createDirectory path
    renderSvgFiles path mosaicSample
