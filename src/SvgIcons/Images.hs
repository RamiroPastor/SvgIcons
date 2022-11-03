

{- |
This module is only for re-exports and documentation.

Unlike icons, all svg from Images modules are already
wrapped with @\<svg\>@ tag, with the appropriate viewbox.
This means that they are not composible, but you don't have to 
prepare the images yourself.

Height to width ratios vary for every image. In particular, 
all flags follow their official proportions; and ratios for mosaics
are specified for each mosaic.

__Note:__ In modern browsers, nesting some @\<svg\>@ inside 
another @\<svg\>@ tag DOES work, but the mechanics of the nested 
viewbox and dimensions are not trivial. Do it under your own responsibility.

To fully grasp the mosaics, check this page: [https://ramiropastor.es/svg-icons](https://ramiropastor.es/svg-icons)
-}
module SvgIcons.Images 
  ( module SvgIcons.Images.CountryFlags
  , module SvgIcons.Images.Mosaics
  ) where


import SvgIcons.Images.CountryFlags
import SvgIcons.Images.Mosaics