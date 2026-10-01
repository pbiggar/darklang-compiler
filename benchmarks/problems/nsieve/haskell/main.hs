-- Parameterized reference workload; see benchmarks/IMPLEMENTATIONS.md for provenance.
{-# LANGUAGE BangPatterns #-}
import Data.Array.IO
import System.Environment (getArgs)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (foldl')

argument :: Int -> IO Int
argument i = fmap (read . (!! i)) getArgs

-- Re-read runtime inputs in each iteration to avoid sharing pure results.
repeatBench :: Int -> IO Int -> IO Int
repeatBench count action = last <$> replicateM count (action >>= evaluate)

sieve :: Int -> IO Int
sieve n = do
  flags <- newArray (0,n) True :: IO (IOUArray Int Bool)
  let mark i j | j > n = return ()
               | otherwise = writeArray flags j False >> mark i (j+i)
      loop i !s | i > n = return s
                | otherwise = do
                    prime <- readArray flags i
                    if prime then mark i (2*i) >> loop (i+1) (s+1) else loop (i+1) s
  loop 2 0
main = do
  rounds <- argument 1
  repeatBench rounds (argument 0 >>= sieve) >>= print
