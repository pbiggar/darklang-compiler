-- Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
{-# LANGUAGE BangPatterns #-}
import Data.Complex
import Data.List (foldl')
import System.Environment
import Control.Monad
modulus = 1000000007 :: Int
fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [x] = [x]
fft xs = zipWith (+) even twiddle ++ zipWith (-) even twiddle
  where
    split [] = ([],[])
    split (a:b:rest) = let (as,bs)=split rest in (a:as,b:bs)
    split _ = error "FFT requires even length"
    (es,os) = split xs
    even = fft es
    odd = fft os
    twiddle = zipWith (\i z -> cis (-2*pi*fromIntegral i/fromIntegral (length xs))*z) [0::Int ..] odd
solve :: Int -> Int
solve n = foldl' (\s (i,z) -> (s+truncate ((realPart z*3+imagPart z*5)*1e6)*(i+1)) `mod` modulus) 0 (zip [0..] result)
  where result=fft [(sin (x*0.017)+cos (x*0.031)) :+ (cos (x*0.013)-sin (x*0.007)) | i<-[0..n-1],let x=fromIntegral i]
main = do
  args <- getArgs
  let runs=read (args!!1)
  values <- replicateM runs $ do
    current <- getArgs
    let !value=solve (read (current!!0))
    pure value
  print (foldl' (\s n -> (s+n) `mod` modulus) 0 values)
