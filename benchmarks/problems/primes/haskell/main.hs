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

isPrime :: Int -> Bool
isPrime n | n < 2 = False
          | n == 2 = True
          | even n = False
          | otherwise = all (\d -> n `mod` d /= 0) [3..floor (sqrt (fromIntegral n :: Double))]
main = argument 0 >>= print . length . filter isPrime . enumFromTo 2
