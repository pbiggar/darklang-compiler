-- Parameterized reference workload; see benchmarks/IMPLEMENTATIONS.md for provenance.
{-# LANGUAGE BangPatterns #-}
import Data.Array.Unboxed
import System.Environment (getArgs)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (foldl')

argument :: Int -> IO Int
argument i = fmap (read . (!! i)) getArgs

-- Re-read runtime inputs in each iteration to avoid sharing pure results.
repeatBench :: Int -> IO Int -> IO Int
repeatBench count action = last <$> replicateM count (action >>= evaluate)

lcg x = (x*1103515245+12345) `mod` 2147483648
matrix n seed = listArray ((0,0),(n-1,n-1)) (map (`mod` 100) (take (n*n) (tail (iterate lcg seed)))) :: UArray (Int,Int) Int
main = do
  n <- argument 0
  let a = matrix n 42
      b = matrix n 123
      c = [sum [a!(i,k) * b!(k,j) | k <- [0..n-1]] | i <- [0..n-1], j <- [0..n-1]]
  print (foldl' (\s (i,x) -> (s+i*x) `mod` 1000000007) 0 (zip [1..] c))
