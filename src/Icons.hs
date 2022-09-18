{-# LANGUAGE     OverloadedStrings       #-}

{- |
This module just recopilates all other icon modules and re-exports them,
so you don't have to import every icon module individually.
You can just @import Icons@ and that will do. 
This is also the perfect place to explain how these icons work. 

You will have to do a little work before using them (explained below),
and when you have the icon with the colors and stroke of your liking, 
you have 2 options:

  (1) If you are using [blaze-html](https://hackage.haskell.org/package/blaze-html)
  for your frontend, everything in this package will work out-of-the-box for you.

  (2) In any other case, you need to render the svg code into some file and work
  with that. The `Core.Render` module provides you with 2 functions to do so, 
  `renderSvgFiles` creates standard @.svg@ files, and `renderSvgReact` creates 
  @.jsx@ files, suitable for the [React.js](https://reactjs.org/) javascript framework.


** Preparing the icon (a little work):

All icons are designed for a @viewbox "-1 -1 2 2"@ (a square centered at (0,0) and side = 2).
However, the viewbox attribute must be assigned to the @\<svg\>@ tag, and the icons return a 
@path@ or @g@ element (to allow composability of icons) so you must wrap it yourself with 
the `svg` function from the @blaze-svg@ package. 

For example, if you want to use the `warning` icon from `Icons.Computer`:

>{-# LANGUAGE     OverloadedStrings       #-}
>
>import           Text.Blaze.Svg11 ((!))
>import           Text.Blaze.Svg11 as S
>import           Text.Blaze.Svg11.Attributes as A
>
>iconExample1 :: Svg
>iconExample1 =
>  S.svg
>    ! A.viewbox "-1 -1 2 2"
>    ! A.width  "200px"
>    ! A.height "300px"
>    $ warning
>      ! A.fill   "pink"
>      ! A.stroke "#777777"
>      ! A.strokeWidth "0.03"

You can render the result into a @.svg@ file and you will get:



-}
module Icons 
  ( exampleIcons
  , iconExample1
  , iconExample2
  , iconExample3
  , module Icons.Business
  , module Icons.Computer
  , module Icons.Cosmos
  , module Icons.Human
  , module Icons.Math
  , module Icons.Office
  , module Icons.Religion
  , module Icons.Textarea
  , module Icons.Tools
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Core.Geometry
import Core.Style
import Icons.Business
import Icons.Computer
import Icons.Cosmos
import Icons.Human
import Icons.Math
import Icons.Office
import Icons.Religion
import Icons.Textarea
import Icons.Tools


{- |
A list with all the examples of this module, 
together with appropriate names.

exampleIcons :: [(String , Svg)]
exampleIcons =
  [ (,) "iconExample1" iconExample1
  ]
-}
exampleIcons :: [(String , Svg)]
exampleIcons =
  [ (,) "iconExample1" iconExample1
  , (,) "iconExample2" iconExample2
  , (,) "iconExample3" iconExample3
  ]


{- |
First example for the documentation above.
-}
iconExample1 :: Svg
iconExample1 =
  S.svg
    ! A.viewbox "-1 -1 2 2"
    ! A.width  "200px"
    ! A.height "300px"
    $ warning
      ! A.fill   "pink"
      ! A.stroke "#777777"
      ! A.strokeWidth "0.03"



{- |
Second example for the documentation above.
-}
iconExample2 :: Svg
iconExample2 = 
  stdDims $ fillStyle accept



{- |
Third example for the documentation above.
-}
iconExample3 :: Svg
iconExample3 =
  S.svg
    ! A.viewbox "-3 -1 6 2"
    ! A.width  "300px"
    ! A.height "100px"
    $ S.g 
      ! A.strokeWidth "0.03"
      $ do
        moonHalf
          ! A.fill   "silver"
          ! A.stroke "gray"
          ! A.transform (translate (-2) 0 <> rotateAround (-30) 0 0 <> S.scale 0.7 0.7)
        sun 11
          ! A.fill   "yellow"
          ! A.stroke "orange"
          ! A.transform (translate ( 2) 0)
        starPolygonFirstSpecies 5 0.5 (0,0)
          ! A.fill   "sandybrown"
          ! A.stroke "crimson"