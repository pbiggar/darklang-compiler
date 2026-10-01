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

steps :: Int -> Int
steps = go 0 where
  go !s 1 = s
  go !s n = go (s+1) (if even n then n `div` 2 else 3*n+1)
main = argument 0 >>= print . (\n -> foldl' (\s i -> s + steps i) 0 [1..n])
