-- Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
{-# LANGUAGE BangPatterns #-}
import System.Environment
import Control.Monad
import Data.List (foldl')
data Vec = Vec !Double !Double !Double
data Sphere = Sphere !Vec !Double !Vec
add (Vec x y z) (Vec a b c)=Vec (x+a) (y+b) (z+c)
sub (Vec x y z) (Vec a b c)=Vec (x-a) (y-b) (z-c)
scale (Vec x y z) s=Vec (x*s) (y*s) (z*s)
dot (Vec x y z) (Vec a b c)=x*a+y*b+z*c
len v=sqrt (dot v v)
normalize v=scale v (1/len v)
intersect origin direction (Sphere center radius _) =
  let offset=sub origin center;b=dot offset direction;c=dot offset offset-radius*radius;d=b*b-c
      root=sqrt d;near= -b-root;far= -b+root
  in if d<0 then Nothing else if near>0.001 then Just near else if far>0.001 then Just far else Nothing
closest origin direction maximum = snd (foldl' step (maximum,Nothing) spheres)
  where step (limit,hit) sphere=case intersect origin direction sphere of
          Just d | d<limit -> (d,Just (d,sphere))
          _ -> (limit,hit)
spheres=[Sphere (Vec 0 0 0) 1 (Vec 0.9 0.22 0.18),Sphere (Vec (-1.45) (-0.35) 1.3) 0.65 (Vec 0.18 0.72 0.30),Sphere (Vec 1.35 0.15 1) 0.8 (Vec 0.18 0.38 0.92),Sphere (Vec 0 (-101) 1.5) 100 (Vec 0.72 0.70 0.62)]
light=Vec (-4) 5 (-3)
trace origin direction = case closest origin direction 1000000 of
  Nothing -> let Vec _ y _=direction;b=0.5*(y+1) in Vec (0.08+0.12*b) (0.10+0.18*b) (0.16+0.30*b)
  Just (distance,Sphere center _ color) ->
    let point=add origin (scale direction distance);normal=normalize (sub point center);surface=add point (scale normal 0.001)
        toward=sub light surface;ld=normalize toward;diffuse=max 0 (dot normal ld)
        intensity=case closest surface ld (len toward) of Just _ -> 0.12;Nothing -> 0.12+0.88*diffuse
    in scale color intensity
render size = foldl' pixel 0 [(x,y) | y<-[0..size-1],x<-[0..size-1]]
  where
    pixel !s (x,y)=let denominator=fromIntegral (size-1)
                       direction=normalize (Vec (2*fromIntegral x/denominator-1) (1-2*fromIntegral y/denominator) 1.5)
                       Vec r g b=trace (Vec 0 0 (-5)) direction
                       value=truncate (r*1e6)*3+truncate (g*1e6)*5+truncate (b*1e6)*7
                   in (s+value*(y*size+x+1)) `mod` 1000000007
main = do
  args <- getArgs
  values <- replicateM (read (args!!1)) $ do
    current <- getArgs
    let !n=render (read (current!!0))
    pure n
  print (foldl' (\s n -> (s+n) `mod` 1000000007) (0::Int) values)
