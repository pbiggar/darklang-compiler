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

-- Adapted from Koka test/bench/koka/tak-int.kk (Apache-2.0).
tak :: Int -> Int -> Int -> Int
tak x y z | y < x = tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
          | otherwise = z
main = do
  rounds <- argument 0
  repeatBench rounds (do
    x <- argument 1
    y <- argument 2
    z <- argument 3
    return (tak x y z)) >>= print
