{-# LANGUAGE     OverloadedStrings       #-}


module Core.Render 
  ( renderSvgFiles
  , renderSvgReact
  , svgToReact
  ) where

import           GHC.IO.Encoding
import qualified Data.Text as T
import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A
import           Text.Blaze.Svg.Renderer.Pretty

import Core.Utils



{- |
`renderSvgFiles` renders svg code to .svg files.

Takes a folder path in your computer and a list
of pairs `(String, Svg)` and renders the svg code of every
second element into a svg file named as the first element.
You should not write the .svg file extension in the first element.
This function also adds the correct DOCTYPE and xml:ns

Example use:

>renderSvgFiles
>  "./assets/img/"
>  [ (,) "sun"      (sun 14)
>  , (,) "moon"      moon
>  , (,) "crescent"  crescent
>  ]

This will create 3 files inside the ./assets/svg/ folder,
namely sun.svg, moon.svg and crescent.svg
-}
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



{- |
`renderSvgReact` renders svg code to .jsx files

Works as the previous function but creates .jsx files
instead of .svg files. This function does not prepend
the svg DOCTYPE but it does prepend the React import and
an export line.

Example use:

>myCancelIcon :: Svg
>myCancelIcon =
>  svg
>    ! viewbox "-1 -1 2 2"
>    $ cancel
>      ! fill "deeppink"
>
>renderSvgReact
>  "./assets/svg/"
>  [ (,) "cancel" myCancelIcon
>  ]

This call will create a cancel.jsx file inside the ./assets/svg/ folder
with the following code inside:

>import React from 'react';
>
>export const cancel =
><svg viewBox="-1 -1 2 2" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
>    <g fill="deeppink">
>        <path d="M 0.1,-0.1 L 0.1,-0.8 A 0.1,0.1 0.0 1,0 -0.1,-0.8 L -0.1,-0.1 L -0.8,-0.1 A 0.1,0.1 0.0 1,0 -0.8,0.1 L -0.1,0.1 L -0.1,0.8 A 0.1,0.1 0.0 1,0 0.1,0.8 L 0.1,0.1 L 0.8,0.1 A 0.1,0.1 0.0 1,0 0.8,-0.1 Z" transform="rotate(45,0,0)" />
>    </g>
></svg>

-}
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



{- |
`svgToReact` is internally used in the previous function
so you don't have to call it manually.

This function writes the @import React from 'react';@ line
and the export line; and more importantly, changes all hyphen-joined
attributes to camelCase, because React will complain otherwise.
For example, @stroke-width@ changes to @strokeWidth@ and @text-anchor@
changes to @textAnchor@.

__IMPORTANT:__ This function does not currently aim to be exhaustive, 
which means that you may encounter some hyphen-joined attribute which
is not converted to camelCase and raises a React error. You can ask
for an update in this function if you have such problem.
-}
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