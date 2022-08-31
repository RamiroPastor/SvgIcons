{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Cosmos where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgCosmos :: [ (String , S.Svg) ]
svgCosmos =
  [ (,) "moon" moon
  ]


--------------------------------------------------------------------------------


moon :: Svg
moon =
    S.path
      ! A.strokeLinejoin "round"
      ! A.d moonDirs
  where
    kx = 0.72
    ky = 0.7
    r1 = 0.92
    r2 = 0.71
    moonDirs = mkPath $ do
      m   ( kx) (-ky)
      aa    r1    r1   0  True  False ( kx) ( ky)
      aa    r2    r2   0  True  True  ( kx) (-ky)
      S.z