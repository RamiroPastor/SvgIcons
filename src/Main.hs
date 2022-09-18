module Main where

import           System.Directory
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S

import Core.Geometry
import Core.Render
import Core.Style
import Core.Utils
import Icons (exampleIcons)
import Icons.Business (svgBusiness)
import Icons.Computer (svgComputer)
import Icons.Cosmos   (svgCosmos)
import Icons.Human    (svgHuman)
import Icons.Math     (svgMath)
import Icons.Office   (svgOffice)
import Icons.Religion (svgReligion)
import Icons.Textarea (svgTextarea)
import Icons.Tools    (svgTools)
import Images.CountryFlags (countryFlags)
import Images.Mosaics      (mosaicSample)






main :: IO ()
main = renderAll "./svg"



renderAll :: FilePath -> IO ()
renderAll svgFolder = do
  createDirectoryIfMissing False svgFolder
  removeDirectoryRecursive       svgFolder
  createDirectory                svgFolder
  renderExamples (svgFolder ++ "/examples/")
  renderIcons    (svgFolder ++ "/icons/")
  renderImages   (svgFolder ++ "/images/")
  renderTest     (svgFolder ++ "/test/") (starRegular 7 0.9 (0,0))
  putStrLn "Svg files compiled correctly"



renderExamples :: FilePath -> IO ()
renderExamples path = do
  createDirectory path
  renderSvgFiles path exampleIcons



renderIcons :: FilePath -> IO ()
renderIcons path = 
  do
    createDirectory path
    createDirectory businessPath
    createDirectory computerPath
    createDirectory cosmosPath
    createDirectory humanPath
    createDirectory mathPath
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
    renderSvgFiles mathPath (map fillIcons svgMath)
    renderSvgFiles mathPath (map fullIcons svgMath)
    renderSvgFiles mathPath (map strkIcons svgMath)
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
    fillIcons (a,b) = (a ++ "_fill" , stdDims $ fillStyle b)
    fullIcons (a,b) = (a ++ "_full" , stdDims $ fullStyle b)
    strkIcons (a,b) = (a ++ "_strk" , stdDims $ strkStyle b)
    -- test (a,b) = (a, coreSvg def $ b >> frame (-1) (-1) 2 2)
    businessPath = path ++ "business/"
    computerPath = path ++ "computer/"
    cosmosPath   = path ++ "cosmos/"
    humanPath    = path ++ "human/"
    mathPath     = path ++ "math/"
    officePath   = path ++ "office/"
    religionPath = path ++ "religion/"
    textareaPath = path ++ "textarea/"
    toolsPath    = path ++ "tools/"



renderImages :: FilePath -> IO ()
renderImages path = do
    createDirectory path
    createDirectory countryFlagsPath
    createDirectory mosaicsPath
    renderSvgFiles countryFlagsPath countryFlags
    renderSvgFiles mosaicsPath      mosaicSample
  where
    countryFlagsPath = path ++ "countryFlags/"
    mosaicsPath      = path ++ "mosaics/"



renderTest :: FilePath -> Svg -> IO ()
renderTest path svgTest = do
    createDirectory path
    renderSvgFiles path test
  where
    test = 
      [ (,) "test_fill" (stdDims $ fillStyle svgFramed)
      , (,) "test_full" (stdDims $ fullStyle svgFramed)
      , (,) "test_strk" (stdDims $ strkStyle svgFramed)
      ]
    svgFramed = 
      S.g $ svgTest >> frame (-1) (-1) 2 2