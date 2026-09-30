-- Parameterized reference workload; see benchmarks/IMPLEMENTATIONS.md for provenance.
{-# LANGUAGE BangPatterns #-}
import Data.Word
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

hashVal :: Word64 -> Word64
hashVal value = foldl' (\h _ -> (h `xor` (value .&. 255))*1099511628211) 14695981039346656037 [1..8 :: Int]
build :: Int -> Word64 -> Word64
build 0 start = hashVal start
build d start = hashVal (build (d-1) start + 31*build (d-1) (start + (1 `shiftL` (d-1))))
main = do
  depth <- argument 0
  rounds <- argument 1
  let loop i !s | i >= rounds = return s
                | otherwise = do
                    root <- evaluate (build depth (fromIntegral i))
                    -- Fresh runtime input forces the verification traversal as well.
                    d <- argument 0
                    verified <- evaluate (build d (fromIntegral i) == root)
                    loop (i+1) ((s + toInteger root + if verified then 1 else 0) `mod` 1000000007)
  loop 0 0 >>= print
