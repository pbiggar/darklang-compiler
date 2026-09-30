-- Parameterized reference workload; see benchmarks/IMPLEMENTATIONS.md for provenance.
{-# LANGUAGE BangPatterns #-}
import Foreign
import Control.Monad (forM_, replicateM_, when)
import System.Environment (getArgs)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (foldl')

argument :: Int -> IO Int
argument i = fmap (read . (!! i)) getArgs

-- Re-read runtime inputs in each iteration to avoid sharing pure results.
repeatBench :: Int -> IO Int -> IO Int
repeatBench count action = last <$> replicateM count (action >>= evaluate)

-- Adapted from the Benchmarks Game's Haskell spectral norm (see provenance).
type Reals = Ptr Double
main = do
  n <- argument 0
  rounds <- argument 1
  allocaArray n $ \u -> allocaArray n $ \v -> allocaArray n $ \tmp -> do
    forM_ [0..n-1] $ \i -> pokeElemOff u i 1 >> pokeElemOff v i 0
    replicateM_ rounds $ do
      timesAv n u tmp 0 n
      timesAtv n tmp v 0 n
      timesAv n v tmp 0 n
      timesAtv n tmp u 0 n
    result <- eigenvalue n u v 0 0 0
    print (truncate (result * 1000000000) :: Int)
aij :: Int -> Int -> Double
aij i j = 1 / fromIntegral ((i+j)*(i+j+1) `div` 2+i+1)
eigenvalue :: Int -> Reals -> Reals -> Int -> Double -> Double -> IO Double
eigenvalue !n !u !v !i !vBv !vv
    | i < n     = do    ui <- peekElemOff u i
                        vi <- peekElemOff v i
                        eigenvalue n u v (i+1) (vBv + ui * vi) (vv + vi * vi)
    | otherwise = return $! sqrt $! vBv / vv

------------------------------------------------------------------------

timesAv :: Int -> Reals -> Reals -> Int -> Int -> IO ()
timesAv !n !u !au !l !r = go l where
    go :: Int -> IO ()
    go !i = when (i < r) $ do
        let avsum !j !acc
                | j < n = do
                        !uj <- peekElemOff u j
                        avsum (j+1) (acc + ((aij i j) * uj))
                | otherwise = pokeElemOff au i acc >> go (i+1)
        avsum 0 0

timesAtv :: Int -> Reals -> Reals -> Int -> Int -> IO ()
timesAtv !n !u !a !l !r = go l
  where
    go :: Int -> IO ()
    go !i = when (i < r) $ do
        let atvsum !j !acc
                | j < n = do    !uj <- peekElemOff u j
                                atvsum (j+1) (acc + ((aij j i) * uj))
                | otherwise = pokeElemOff a i acc >> go (i+1)
        atvsum 0 0
