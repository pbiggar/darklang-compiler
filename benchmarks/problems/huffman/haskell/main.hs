-- Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
{-# LANGUAGE BangPatterns #-}
import Data.List (sortOn,foldl')
import qualified Data.IntMap.Strict as M
import System.Environment
import Control.Monad
modulus=1000000007 :: Int
data Tree = Leaf !Int | Branch !Tree !Tree deriving Show
generate n seed = take n $ map choose $ drop 1 $ iterate (\s -> (s*1103515245+12345) `mod` 2147483648) seed
  where choose s = case [i | (i,t)<-zip [0..] [300,480,610,710,790,850,900,940], s `mod` 1000<t] of
                     i:_ -> i
                     [] -> 8+s `mod` 24
codec xs = combine (sortOn key [(w,s,Leaf s) | (s,w)<-M.toList frequencies])
  where
    frequencies=M.fromListWith (+) [(s,1) | s<-xs]
    key (w,s,_)=(w,s)
    combine [(_,_,tree)]=tree
    combine ((w,s,a):(v,t,b):rest)=combine (sortOn key ((w+v,min s t,Branch a b):rest))
    combine _=error "empty codec"
codes tree = M.fromList (walk tree 0 0)
  where walk (Leaf s) bits len=[(s,(bits,max 1 len))]
        walk (Branch a b) bits len=walk a (bits*2) (len+1)++walk b (bits*2+1) (len+1)
encode table xs=concatMap (\s -> let (b,l)=table M.! s in [b `div` (2^i) `mod` 2 | i<-[l-1,l-2..0]]) xs
decode root bits = case root of
    Leaf s -> replicate (length bits) s
    _ -> walk root bits
  where walk _ []=[]
        walk (Leaf s) rest=s:walk root rest
        walk (Branch a b) (v:rest)=case if v==0 then a else b of
          Leaf s -> s:walk root rest
          next -> walk next rest
checksum xs=foldl' (\s (i,v) -> (s+i*v) `mod` modulus) 0 (zip [1..] xs)
runCodec tree table xs = if decoded/=xs then error "Huffman roundtrip failed" else (length encoded*17+checksum encoded+checksum decoded) `mod` modulus
  where encoded=encode table xs;decoded=decode tree encoded
main = do
  args <- getArgs
  let xs=generate (read (args!!0)) (read (args!!1));tree=codec xs;table=codes tree
  values <- forM [1..(read (args!!2)::Int)] $ \_ -> do
    -- Runtime input forces a fresh encode/decode while the codec stays shared.
    current <- getArgs
    let input=take (read (current!!0)) xs
        !n=runCodec tree table input
    pure n
  print (foldl' (\s n -> (s+n) `mod` modulus) 0 values)
