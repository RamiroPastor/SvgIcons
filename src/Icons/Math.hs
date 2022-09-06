{-# LANGUAGE     OverloadedStrings       #-}



module Icons.Math where

import           Text.Blaze.Svg11 ((!))
import           Text.Blaze.Svg11 as S
import           Text.Blaze.Svg11.Attributes as A

import Base



svgMath :: [ (String , S.Svg) ]
svgMath =
  [ (,) "lambda"     lambda
  , (,) "lemniscate" lemniscate
  ]


--------------------------------------------------------------------------------

lambda :: S.Svg
lambda = 
    S.g $ do
      S.path
        ! A.strokeLinejoin "round"
        ! A.d (mkPath $ rightLeg >> leftLeg >> arm)
        ! A.transform (translate 0 (-0.02))
  where
    (c1,c2) = (,) ( 0    ) ( 0    )
    (a1,a2) = (,) (-0.376) ( 0.962)
    (b1,b2) = (,) (-0.548) ( a2   )
    (d1,d2) = (,) ( 0.088) (-0.098)
    leftLeg = do
      S.l c1 c2
      S.l a1 a2
      S.l b1 b2
      S.l m1 m2
    (e1,e2) = (,) ( 0.226) ( 0.54 )
    (f1,f2) = (,) ( 0.326) ( 0.864)
    (g1,g2) = (,) ( 0.610) ( 0.890)
    (h1,h2) = (,) ( 0.652) ( 0.576)
    (j1,j2) = (,) ( 0.710) ( 1.10 )
    (k1,k2) = (,) ( 0.234) ( j2   )
    (l1,l2) = (,) ( 0.142) ( 0.60 )
    (m1,m2) = (,) (-0.054) (-0.274)
    rightLeg = do
      S.m d1 d2
      S.l e1 e2
      S.c f1 f2 g1 g2 h1 h2
      S.c j1 j2 k1 k2 l1 l2
    (n1,n2) = (,) (-0.12 ) (-0.86 )
    (o1,o2) = (,) (-0.470) ( n2   )
    (p1,p2) = (,) (-0.550) (-0.502)
    (r1,r2) = (,) (-0.570) (-1.06 )
    (s1,s2) = (,) (-0.142) ( r2 )
    (t1,t2) = (,) (-0.04 ) (-0.66 )
    arm = do
      S.c n1 n2 o1 o2 p1 p2
      S.c r1 r2 s1 s2 t1 t2
      S.l d1 d2

  
lemniscate :: Svg
lemniscate = 
    S.g $ do
      S.path
        ! A.fill "none"
        ! A.d dirs
  where
    k = 0.5
    r = 0.4
    dirs = mkPath $ do
      m  (-k) (-r)
      aa   r    r    0    True  False  (-k) ( r)
      c    0    r    0    (-r)         ( k) (-r)
      aa   r    r    0    True  True   ( k) ( r)
      c    0    r    0    (-r)         (-k) (-r)
      S.z