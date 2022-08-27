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
import Render





main :: IO ()
main = renderAll "./svg"



renderAll :: FilePath -> IO ()
renderAll svgFolder = do
  createDirectoryIfMissing False svgFolder
  removeDirectoryRecursive       svgFolder
  createDirectory                svgFolder
  renderIcons   (svgFolder ++ "/icons")
  renderMosaics (svgFolder ++ "/mosaics")
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
    renderSvgFile businessPath (map fillIcons svgBusiness)
    renderSvgFile businessPath (map fullIcons svgBusiness)
    renderSvgFile businessPath (map strkIcons svgBusiness)
    renderSvgFile filePath (map fillIcons svgFile)
    renderSvgFile filePath (map fullIcons svgFile)
    renderSvgFile filePath (map strkIcons svgFile)
    renderSvgFile humanPath (map fillIcons svgHuman)
    renderSvgFile humanPath (map fullIcons svgHuman)
    renderSvgFile humanPath (map strkIcons svgHuman)
    renderSvgFile officePath (map fillIcons svgOffice)
    renderSvgFile officePath (map fullIcons svgOffice)
    renderSvgFile officePath (map strkIcons svgOffice)
    renderSvgFile textareaPath (map fillIcons svgTextarea)
    renderSvgFile textareaPath (map fullIcons svgTextarea)
    renderSvgFile textareaPath (map strkIcons svgTextarea)
    renderSvgFile toolsPath (map fillIcons svgTools)
    renderSvgFile toolsPath (map fullIcons svgTools)
    renderSvgFile toolsPath (map strkIcons svgTools)
  where
    fillIcons (a,b) = (a ++ "_fill" , coreSvg def $ applyStyle fillStyle   b)
    fullIcons (a,b) = (a ++ "_full" , coreSvg def $ applyStyle fullStyle   b)
    strkIcons (a,b) = (a ++ "_strk" , coreSvg def $ applyStyle strkStyle b)
    -- test (a,b) = (a, coreSvg def $ b >> frame (-1) (-1) 2 2)
    businessPath = path ++ "/business/"
    filePath     = path ++ "/file/"
    humanPath    = path ++ "/human/"
    officePath   = path ++ "/office/"
    textareaPath = path ++ "/textarea/"
    toolsPath    = path ++ "/tools/"
    


renderMosaics :: FilePath -> IO ()
renderMosaics path = do
    createDirectory path
    putStrLn "No mosaics yet"
