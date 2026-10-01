-- Parameterized reference workload; see benchmarks/IMPLEMENTATIONS.md for provenance.
{-# LANGUAGE BangPatterns #-}
import Data.Bits
import System.Environment (getArgs)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (foldl')

argument :: Int -> IO Int
argument i = fmap (read . (!! i)) getArgs

-- Re-read runtime inputs in each iteration to avoid sharing pure results.
repeatBench :: Int -> IO Int -> IO Int
repeatBench count action = last <$> replicateM count (action >>= evaluate)

queens :: Int -> Int
queens n = place 0 0 0 where
  allBits :: Int
  allBits = (1 `shiftL` n) - 1
  place cols left right
    | cols == allBits = 1
    | otherwise = choose (allBits .&. complement (cols .|. left .|. right)) 0
    where
      choose 0 !s = s
      choose avail !s = let bit = avail .&. negate avail in
        choose (avail `xor` bit) (s + place (cols .|. bit) ((left .|. bit) `shiftL` 1) ((right .|. bit) `shiftR` 1))
main = argument 0 >>= print . queens
