{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Cosmos where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgCosmos :: [ (String , S.Svg) ]
svgCosmos =
  [ (,) "sun"      (sun 14)
  , (,) "moon"      moon
  , (,) "crescent"  crescent
  ]


--------------------------------------------------------------------------------

-- n is half the number of rays
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

  
crescent :: Svg
crescent =
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