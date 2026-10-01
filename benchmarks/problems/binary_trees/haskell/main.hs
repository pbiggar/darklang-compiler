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

-- Adapted from Don Stewart, Roman Kashitsyn and Izaak Weiss (Benchmarks Game).
data Tree = Nil | Node !Tree !Tree
make :: Int -> Int -> Tree
make _ 0 = Node Nil Nil
make !salt d = Node (make (salt-1) (d-1)) (make (salt+1) (d-1))
check :: Tree -> Int
check t = go t 0 where
  go Nil !a = a
  go (Node l r) !a = go l (go r (a+1))
main = do
  d <- argument 0
  rounds <- argument 1
  counts <- mapM (\i -> evaluate (check (make i d))) [1..rounds]
  print (sum counts)
