{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Cosmos 
  ( svgCosmos
  , moonCrescent
  , moonFull
  , sun
  ) where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Core.Utils



{- |
A list with all the icons of this module, 
together with appropriate names.

>svgCosmos :: [ (String , S.Svg) ]
>svgCosmos =
>  [ (,) "moonCrescent"  moonCrescent
>  , (,) "moonFull"      moonFull
>  , (,) "sun"          (sun 14)
>  ]
-}
svgCosmos :: [ (String , S.Svg) ]
svgCosmos =
  [ (,) "moonCrescent"  moonCrescent
  , (,) "moonFull"      moonFull
  , (,) "sun"          (sun 14)
  ]


--------------------------------------------------------------------------------



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/sun_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/sun_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/sun_strk.svg)

Takes a natural number @n@ which draws @2*n@ rays.
-}
sun :: Int -> Svg
sun n =
    S.g $ do
      S.circle
        ! A.x "0"
        ! A.y "0"
        ! A.r "0.5"
      S.path
        ! A.strokeLinecap "round"
        ! A.d rays
  where
    r1 = 0.6
    r2 = 0.78
    r3 = 0.96
    α  = 2*pi / (fromIntegral n)
    angles = [ n * α | n <- [0 .. (2*pi / α)]]
    rays = 
      mkPath $ mapM_ doubleRay angles
    doubleRay β = do
      ray r2 β
      ray r3 (β + α/2)
    ray r β = do
      m   (r1 * cos β)  (r1 * sin β)
      l   (r  * cos β)  (r  * sin β)



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/moonFull_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/moonFull_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/moonFull_strk.svg)
-}
moonFull :: Svg
moonFull =
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



{- |
![fill style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/moonCrescent_fill.svg)

![fill and stroke](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/moonCrescent_full.svg)

![stroke style](https://raw.githubusercontent.com/RamiroPastor/SvgIcons/main/svg/icons/cosmos/moonCrescent_strk.svg)
-}
moonCrescent :: Svg
moonCrescent =
    S.path
      ! A.strokeLinejoin "round"
      ! A.d moonDirs
  where
    kx = 0.55
    ky = 0.55
    r1 = 0.8
    r2 = 0.65
    moonDirs = mkPath $ do
      m   ( kx) (-ky)
      aa    r1    r1   0  True  False ( kx) ( ky)
      aa    r2    r2   0  True  True  ( kx) (-ky)
      S.z