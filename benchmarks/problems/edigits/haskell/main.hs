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

digits :: Int -> [Int]
digits n = take n (snd (foldl' step (initial, initial) [1..50])) where
  initial = 1 : replicate (n+10) 0
  divide k = snd . foldl' (\(carry,xs) d -> let (q,r) = (carry*10+d) `quotRem` k in (r,xs++[q])) (0,[])
  add xs ys = snd (foldl' (\(carry,ds) (a,b) -> let (q,r) = (a+b+carry) `quotRem` 10 in (q,r:ds)) (0,[]) (reverse (zip xs ys)))
  step (term,total) k = let t = divide k term in (t,add total t)
checksum n = foldl' (\s (i,d) -> (s+i*d) `mod` 1000000007) 0 (zip [1..] (digits n))
main = do
  rounds <- argument 0
  repeatBench rounds (checksum <$> argument 1) >>= print
