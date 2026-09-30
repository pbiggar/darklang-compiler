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
series n = foldl' (\s k -> s + 1/fromIntegral (k*k)) 0 [1..n]
main = do
  rounds <- argument 0
  repeatBench rounds (fmap (\n -> truncate (series n * 1000000000000)) (argument 1)) >>= print
