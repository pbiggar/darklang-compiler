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

alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"
cumulative = tail . scanl (+) 0
randomFasta :: Int -> [(Int,Double)] -> Int -> (Int,Int)
randomFasta n table seed = go 1 seed 0 where
  go i state !s | i > n = (s,state)
                | otherwise = let next = (state*3877+29573) `mod` 139968
                                  r = fromIntegral next/139968
                                  c = fst (head (dropWhile (\(_,p) -> r >= p) table))
                              in go (i+1) next ((s+c*i) `mod` 1000000007)
main = do
  n <- argument 0
  let iub = zip (map fromEnum "acgtBDHKMNRSVWY") (cumulative ([0.27,0.12,0.12,0.27]++replicate 11 0.02))
      human = zip (map fromEnum "acgt") (cumulative [0.3029549426680,0.1979883004921,0.1975473066391,0.3015094502008])
      c1 = foldl' (\s i -> (s+fromEnum (alu!!((i-1) `mod` length alu))*i) `mod` 1000000007) 0 [1..2*n]
      (c2,seed) = randomFasta (3*n) iub 42
      (c3,_) = randomFasta (5*n) human seed
  print ((c1+c2+c3) `mod` 1000000007)
