

{- |
This module is only for re-exports and documentation.

Unlike icons, all svg from Images modules are already
wrapped with @\<svg\>@ tag, with the appropriate viewbox.
This means that they are not composible, but you don't have to 
prepare the images yourself.

Height to width ratios vary for every image. In particular, 
all flags follow their official proportions; and ratios for mosaics
are specified for each mosaic.
-}
module SvgIcons.Images 
  ( module SvgIcons.Images.CountryFlags
  , module SvgIcons.Images.Logos
  , module SvgIcons.Images.Mosaics
  ) where


import SvgIcons.Images.CountryFlags
import SvgIcons.Images.Logos
import SvgIcons.Images.Mosaics