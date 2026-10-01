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

sumTo :: Int -> Int
sumTo n = go n 0 where
  go 0 !s = s
  go k !s = go (k-1) (s+k)
main = do
  rounds <- argument 0
  repeatBench rounds (sumTo <$> argument 1) >>= print
