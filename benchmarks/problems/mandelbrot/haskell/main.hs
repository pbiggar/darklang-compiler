-- Parameterized reference workload; see benchmarks/IMPLEMENTATIONS.md for provenance.
{-# LANGUAGE BangPatterns #-}
import System.Environment (getArgs)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (foldl')

argument :: Int -> IO Int
argument i = fmap (read . (!! i)) getArgs

-- Re-read runtime inputs in each iteration to avoid sharing pure results.
repeatBench :: Int -> IO Int -> IO Int
repeatBench count action = last <$> replicateM count (action >>= evaluate)

inside :: Int -> Double -> Double -> Int
inside limit cr ci = go 0 0 0 where
  go i zr zi
    | i >= limit = 1
    | zr*zr + zi*zi > 4 = 0
    | otherwise = go (i+1) (zr*zr-zi*zi+cr) (2*zr*zi+ci)
main = do
  n <- argument 0
  limit <- argument 1
  print (sum [inside limit (2*fromIntegral x/fromIntegral n-1.5) (2*fromIntegral y/fromIntegral n-1) | y <- [0..n-1], x <- [0..n-1]])
