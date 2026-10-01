-- Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
{-# LANGUAGE BangPatterns #-}
import qualified Data.IntMap.Strict as M
import Data.Array.Unboxed
import Data.List (foldl')
import System.Environment
import Control.Monad
modulus=1000000007 :: Int
diff left right = if x0==n && y0==m then (0,w0) else layer 1 (M.singleton 0 x0) w0
  where
    n=length left; m=length right
    a=listArray (0,n-1) left :: UArray Int Char
    b=listArray (0,m-1) right :: UArray Int Char
    snake x y | x<n && y<m && a!x==b!y = snake (x+1) (y+1)
              | otherwise = (x,y)
    (x0,y0)=snake 0 0; w0=(x0+1)*(y0+3)
    layer d prev work =
      let step (!front,!w,!reached) k =
            let start=if k==(-d) || (k/=d && prev M.! (k-1)<prev M.! (k+1)) then prev M.! (k+1) else prev M.! (k-1)+1
                (x,y)=snake start (start-k)
            in (M.insert k x front,(w+(x+1)*(y+3)+(k+d+1)*17) `mod` modulus,reached || (x>=n && y>=m))
          (next,w,done)=foldl' step (M.empty,work,False) [-d,-d+2..d]
      in if done then (d,w) else layer (d+1) next w
solve blocks insertions = (d*1000003+w) `mod` modulus
  where
    unit="darklang compiler benchmark: persistent values and recursive paths.\n"
    prefix=concat (replicate blocks unit);suffix=concat (replicate (blocks+1) unit)
    (d,w)=diff (prefix++suffix) (prefix++concat (replicate insertions "<changed-block>")++suffix)
main = do
  args <- getArgs
  values <- replicateM (read (args!!2)) $ do
    current <- getArgs
    let !n=solve (read (current!!0)) (read (current!!1))
    pure n
  print (foldl' (\s n -> (s+n) `mod` modulus) 0 values)
