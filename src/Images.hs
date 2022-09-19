

{- |
This module is only for re-exports and documentation.

Unlike icons, all svg from Images modules are already
already wrapped with @\<svg\>@ tag, with the appropriate viewbox.
This means that they are not composible, but you don't have to 
prepare the images yourself.

Height to width ratios vary for every image. In particular, 
all flags follow their official proportions; and ratios for mosaics
are specified for each mosaic.
-}
module Images 
  ( module Images.CountryFlags
  , module Images.Mosaics
  ) where


import Images.CountryFlags
import Images.Mosaics