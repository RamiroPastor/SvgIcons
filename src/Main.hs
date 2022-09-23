

{- |
The main purpose of this module is 
generating the images found within the haddock documentation
of this package.
-}
module Main where

import           System.Directory
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S

import SvgIcons.Core.Geometry
import SvgIcons.Core.Render
import SvgIcons.Core.Style
import SvgIcons.Core.Utils
import SvgIcons.Icons (exampleIcons)
import SvgIcons.Icons.Business (svgBusiness)
import SvgIcons.Icons.Computer (svgComputer)
import SvgIcons.Icons.Cosmos   (svgCosmos)
import SvgIcons.Icons.Human    (svgHuman)
import SvgIcons.Icons.Math     (svgMath)
import SvgIcons.Icons.Office   (svgOffice)
import SvgIcons.Icons.Religion (svgReligion)
import SvgIcons.Icons.Textarea (svgTextarea)
import SvgIcons.Icons.Tools    (svgTools)
import SvgIcons.Images.CountryFlags (countryFlags)
import SvgIcons.Images.Mosaics      (mosaicSample)





{- |
prop> main = renderAll "./svg"
-}
main :: IO ()
main = renderAll "./svg"


{- |
Renders all icons and images from this package into 
the target directory. This is used to generate the 
example SVGs found in this documentation.

The directory is created if it does not exist, and
some subdirectories are created to distinguish between
icons or images.

__WARNING:__ this function __deletes the target directory__
and then creates it again. Be careful.
-}
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


{- |
Renders the examples from `Icons` module
into the targeted directory.
-}
renderExamples :: FilePath -> IO ()
renderExamples path = do
  createDirectory path
  renderSvgFiles path exampleIcons



{- |
Renders all icons into the 
targeted directory.

All icons are rendered in a @viewbox "-1 -1 2 2"@ and
with 3 style variants:

  * Black fill and no stroke
  * Black stroke and no fill
  * Silver fill and black stroke
-}
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



{- |
Renders all images into the targeted directory.
-}
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



{- |
Takes an icon as second argument and renders it with all
3 styles (fill, full and stroke) into the targeted directory.

The `frame` function is used for testing purposes.
-}
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