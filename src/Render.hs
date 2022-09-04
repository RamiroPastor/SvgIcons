{-# LANGUAGE     OverloadedStrings       #-}


module Render where

import           GHC.IO.Encoding
import qualified Data.Text as T
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A
import           Text.Blaze.Svg.Renderer.Pretty

import Base




renderSvgFiles :: FilePath -> [ (FilePath , Svg) ] -> IO ()
renderSvgFiles folder svgs = 
  do
    setLocaleEncoding utf8
    mapM_ f svgs
  where
    f (name , svgCode) =
      writeFile 
        (folder ++ name ++ ".svg") 
        (renderSvg $ S.docType >> addXmlns svgCode)


renderSvgReact :: FilePath -> [ (FilePath , Svg) ] -> IO ()
renderSvgReact folder svgs =
  do
    setLocaleEncoding utf8
    mapM_ f svgs
  where
    f (name , svgCode) =
      writeFile 
        (folder ++ name ++ ".jsx") 
        (svgToReact name $ addXmlns svgCode)





svgToReact :: String -> Svg -> String
svgToReact name svgCode =
    "import React from 'react';"
    ++ "\n\n" ++
    "export const " ++ name ++ " = \n" ++ render svgCode
  where
    render = T.unpack . adaptToReact . T.pack . renderSvg 
    adaptToReact =
        (T.replace "xmlns:xlink"       "xmlnsXlink")
      . (T.replace "stroke-width"      "strokeWidth")
      . (T.replace "stroke-dasharray"  "strokeDasharray")
      . (T.replace "stroke-dashoffset" "strokeDashoffset")
      . (T.replace "stroke-linejoin"   "strokeLinejoin")
      . (T.replace "stroke-linecap"    "strokeLinecap")
      . (T.replace "font-family"       "fontFamily")
      . (T.replace "font-size"         "fontSize")
      . (T.replace "text-anchor"       "textAnchor")
      . (T.replace "letter-spacing"    "letterSpacing")
      . (T.replace "dominant-baseline" "dominantBaseline")
      . (T.replace "stroke-miterlimit" "strokeMiterlimit")