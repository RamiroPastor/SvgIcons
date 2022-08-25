{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Office where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgOffice :: [ (String , S.Svg) ]
svgOffice =
  [ (,) "envelope" envelope
  ]


--------------------------------------------------------------------------------





envelope :: Svg
envelope =
    S.path
      ! d dirs
      ! strokeLinejoin "round"
  where
    kx = 0.94
    ky = 0.618 * kx
    mx = 0
    my = 0
    s  = 0.03
    dirs = mkPath $ do
      m   (-kx)  (-ky)
      l   ( mx)      ( my)
      l   ( kx)  (-ky)
      S.z
      m   (-kx)      (-ky + s)
      l   (-kx)      ( ky)
      l   ( kx)      ( ky)
      l   ( kx)      (-ky + s)
      l   ( mx)      ( my + s)
      S.z
      