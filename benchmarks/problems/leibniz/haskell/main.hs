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

series :: Int -> Double
series n = go 0 0 where
  go i !s | i >= n = 4*s
          | otherwise = go (i+1) (s + (if even i then 1 else -1)/fromIntegral (2*i+1))
main = argument 0 >>= print . (\n -> truncate (series n * 100000000) :: Int)
