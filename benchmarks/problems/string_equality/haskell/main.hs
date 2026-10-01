-- Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
{-# LANGUAGE BangPatterns #-}
import System.Environment
import Control.Monad
import Data.List (foldl')
score token = sum [weight | (left,other,weight)<-cases,left==other]
  where
    middle=concat (replicate 2 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
    short="ab"++token++"cd"
    long="prefix:"++token++":"++middle++":suffix"
    shortCases=[short,concat ["a","b",token,"cd"],short++"x","xb"++token++"cd","ab"++token++"ce"]
    longCases=[long,concat ["pre","fix:",token,":",middle,":suffix"],long++"!","xrefix:"++token++":"++middle++":suffix","prefix:"++token++":"++middle++":suffiy"]
    cases=zipWith (\x w -> (short,x,w)) shortCases [1,2,4,8,16] ++ zipWith (\x w -> (long,x,w)) longCases [32,64,128,256,512]
main = do
  args <- getArgs
  values <- replicateM (read (args!!0)) $ do
    current <- getArgs
    let !n=score (current!!1)
    pure n
  print (foldl' (+) (0::Int) values)
