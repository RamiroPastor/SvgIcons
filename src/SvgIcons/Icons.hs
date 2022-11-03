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


== Preparing the icon:

All icons are designed for a @viewbox "-1 -1 2 2"@ (a square centered at (0,0) and side 2).
However, the viewbox attribute must be assigned to the @\<svg\>@ tag, and the icons return a 
@path@ or @g@ element (to allow composability of icons) so you must wrap it yourself with 
the `svg` function from the @blaze-svg@ package. 

For example, if you want to use the `warning` icon from `Icons.Computer`:

>
>{-# LANGUAGE     OverloadedStrings       #-}
>
>import           Text.Blaze.Svg11 ((!))
>import           Text.Blaze.Svg11 as S
>import           Text.Blaze.Svg11.Attributes as A
>
>import SvgIcons.Icons.Computer (warning)
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

![icon example 1](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/iconExample1.svg)

Note that the functions in the previous example come from 
the [blaze-svg](https://hackage.haskell.org/package/blaze-svg) package, 
so you will need to add it to your dependencies.

There are some functions defined in module `Core.Style` that can make
this process easier, in particular `stdDims`. Remember that you can avoid
directly coding @fill@, @stroke@, @stroke-width@, @width@ or @height@ (and more)
if you plan to use the svg inline (directly in your HTML code) and set those 
properties with CSS (but it won't work for @\<img\>@ tags)

Example with Core.Style:

>import SvgIcons.Core.Style
>import SvgIcons.Icons.Computer (accept)
>
>iconExample2 :: Svg
>iconExample2 = 
>  stdDims $ fillStyle accept

Resulting svg:

![icon example 2](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/iconExample2.svg)

All icon images in the documentation have been generated using
the three style functions found in `Core.Style`, to expose how each icon looks
with only stroke and no fill, only fill and no stroke, and both fill and stroke ("full" style).
Most of them work with all 3 styles, but some are of no use in one of the three cases.

== Composing Icons

As said before, all icons aim to be composable (mixing icons in the same svg element).
Again, this is achieved by not wrapping them in the @\<svg\>@ tag in their definitions,
and leaving the outmost tag of every icon (usually a @path@ or a @g@ element) free of 
any transform attribute (and free of almost every other attribute too). Even those icons that
have some transformation inside their code already, are wrapped with a clean @\<g\>@ tag to allow 
for further transformations.

Example:

(remember that svg transformations are correctly applied from right to left, 
as it should be because it's just function composition)

>\{\-\# LANGUAGE     OverloadedStrings       \#\-\}
>
>import           Text.Blaze.Svg11 ((!))
>import           Text.Blaze.Svg11 as S
>import           Text.Blaze.Svg11.Attributes as A
>
>import SvgIcons.Core.Geometry (starPolygonFirstSpecies)
>import SvgIcons.Icons.Cosmos (moonHalf, sun)
>
>
>iconExample3 :: Svg
>iconExample3 =
>  S.svg
>    ! A.viewbox "-3 -1 6 2"
>    ! A.width  "300px"
>    ! A.height "100px"
>    $ S.g 
>      ! A.strokeWidth "0.03"
>      $ do
>        moonHalf
>          ! A.fill   "silver"
>          ! A.stroke "gray"
>          ! A.transform (translate (-2) 0 <> rotateAround (-30) 0 0 <> S.scale 0.7 0.7)
>        sun 11
>          ! A.fill   "yellow"
>          ! A.stroke "orange"
>          ! A.transform (translate ( 2) 0)
>        starPolygonFirstSpecies 5 0.5 (0,0)
>          ! A.fill   "sandybrown"
>          ! A.stroke "crimson"

Resulting svg composition:

![icon example 2](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/examples/iconExample3.svg)

== Multiple Fill Colors

Currently, there is no way to fill different parts of one icon with different colors
(or more exotic, animate specific parts of some icon).
I am reluctant to parametrize colors as arguments for every icon, but this could be achieved
by giving a unique id or class to each component of every icon, so it can be controlled with CSS.

This is not on my priority list
at this moment, but do not hesitate to ask for it if you need it. Or even better, do it yourself and
ask for a branch merge on github.

-}
module SvgIcons.Icons 
  ( exampleIcons
  , iconExample1
  , iconExample2
  , iconExample3
  -- * Re-export all Icons modules
  , module SvgIcons.Icons.Business
  , module SvgIcons.Icons.Coding
  , module SvgIcons.Icons.Computer
  , module SvgIcons.Icons.Cosmos
  , module SvgIcons.Icons.Human
  , module SvgIcons.Icons.Math
  , module SvgIcons.Icons.Office
  , module SvgIcons.Icons.Religion
  , module SvgIcons.Icons.Textarea
  , module SvgIcons.Icons.Tools
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import SvgIcons.Core.Geometry
import SvgIcons.Core.Style
import SvgIcons.Icons.Business
import SvgIcons.Icons.Coding
import SvgIcons.Icons.Computer
import SvgIcons.Icons.Cosmos
import SvgIcons.Icons.Human
import SvgIcons.Icons.Math
import SvgIcons.Icons.Office
import SvgIcons.Icons.Religion
import SvgIcons.Icons.Textarea
import SvgIcons.Icons.Tools


{- |
A list with all the examples of this module, 
together with appropriate names.

>exampleIcons :: [(String , Svg)]
>exampleIcons =
>  [ (,) "iconExample1" iconExample1
>  , (,) "iconExample2" iconExample2
>  , (,) "iconExample3" iconExample3
>  ]

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