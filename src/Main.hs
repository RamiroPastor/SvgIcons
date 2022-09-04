module Main (main) where

import Data.Default
import System.Directory
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S

import Base
import Core
import Geometry
import Icons.Business (svgBusiness)
import Icons.Computer (svgComputer)
import Icons.Cosmos   (svgCosmos)
import Icons.Human    (svgHuman)
import Icons.Office   (svgOffice)
import Icons.Religion (svgReligion)
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
  renderTest    (svgFolder ++ "/test/") (regularPolygon 5 0.5 (0.2,0.3))
  putStrLn "Svg files compiled correctly"



renderIcons :: FilePath -> IO ()
renderIcons path = 
  do
    createDirectory path
    createDirectory businessPath
    createDirectory computerPath
    createDirectory cosmosPath
    createDirectory humanPath
    createDirectory officePath
    createDirectory religionPath
    createDirectory textareaPath
    createDirectory toolsPath
    renderSvgFiles businessPath (map fillIcons svgBusiness)
    renderSvgFiles businessPath (map fullIcons svgBusiness)
    renderSvgFiles businessPath (map strkIcons svgBusiness)
    renderSvgFiles computerPath (map fillIcons svgComputer)
    renderSvgFiles computerPath (map fullIcons svgComputer)
    renderSvgFiles computerPath (map strkIcons svgComputer)
    renderSvgFiles cosmosPath (map fillIcons svgCosmos)
    renderSvgFiles cosmosPath (map fullIcons svgCosmos)
    renderSvgFiles cosmosPath (map strkIcons svgCosmos)
    renderSvgFiles humanPath (map fillIcons svgHuman)
    renderSvgFiles humanPath (map fullIcons svgHuman)
    renderSvgFiles humanPath (map strkIcons svgHuman)
    renderSvgFiles officePath (map fillIcons svgOffice)
    renderSvgFiles officePath (map fullIcons svgOffice)
    renderSvgFiles officePath (map strkIcons svgOffice)
    renderSvgFiles religionPath (map fillIcons svgReligion)
    renderSvgFiles religionPath (map fullIcons svgReligion)
    renderSvgFiles religionPath (map strkIcons svgReligion)
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
    computerPath = path ++ "computer/"
    cosmosPath   = path ++ "cosmos/"
    humanPath    = path ++ "human/"
    officePath   = path ++ "office/"
    religionPath = path ++ "religion/"
    textareaPath = path ++ "textarea/"
    toolsPath    = path ++ "tools/"
    


renderMosaics :: FilePath -> IO ()
renderMosaics path = do
    createDirectory path
    renderSvgFiles path mosaicSample



renderTest :: FilePath -> Svg -> IO ()
renderTest path svgTest = do
    createDirectory path
    renderSvgFiles path test
  where
    test = 
      [ (,) "test_fill" (coreSvg def $ applyStyle fillStyle svgFramed)
      , (,) "test_full" (coreSvg def $ applyStyle fullStyle svgFramed)
      , (,) "test_strk" (coreSvg def $ applyStyle strkStyle svgFramed)
      ]
    svgFramed = 
      S.g $ svgTest >> frame (-1) (-1) 2 2