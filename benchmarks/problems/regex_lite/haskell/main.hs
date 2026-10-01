-- Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
{-# LANGUAGE BangPatterns #-}
import Data.List (isPrefixOf,foldl',tails)
import Data.Char
import System.Environment
import Control.Monad
-- Small grammar as an algebraic data type, with a set of possible endpoints.
data Atom = Literal Char | Any | Range Char Char deriving Show
data Term = Term Atom Int (Maybe Int) deriving Show
parsePattern :: String -> [[Term]]
parsePattern source = map branch (split source)
  where
    split s = case break (=='|') s of (a,[]) -> [a];(a,_:b) -> a:split b
    branch []=[]
    branch s=let (atom,rest)=case s of
                       '[':a:'-':b:']':tail -> (Range a b,tail)
                       '.':tail -> (Any,tail)
                       c:tail -> (Literal c,tail)
                 (lo,hi,tail)=case rest of
                       '*':r -> (0,Nothing,r)
                       '+':r -> (1,Nothing,r)
                       '?':r -> (0,Just 1,r)
                       r -> (1,Just 1,r)
             in Term atom lo hi:branch tail
matches (Literal a) c=a==c
matches Any _=True
matches (Range a b) c=c>=a && c<=b
run [] _=True
run (Term atom lo hi:terms) text = any (run terms . (`drop` text)) lengths
  where
    count=length (takeWhile (matches atom) text)
    maximum=maybe count (min count) hi
    lengths=[lo..maximum]
solve blocks = count*1000003+checksum
  where
    text=concat (replicate blocks "darklang darkxxlang compiler42 compiler ab ab7 nope DARKlang compilerx\n")
    pattern=parsePattern "dark[a-z]*lang|compiler[0-9]+|ab[0-9]?"
    step (!count,!sum) (i,s) = if any (`run` s) pattern then (count+1,(sum+(i+1)*(count+3)) `mod` 1000000007) else (count,sum)
    (count,checksum)=foldl' step (0,0) (zip [0..length text-1] (tails text))
main = do
  args <- getArgs
  values <- replicateM (read (args!!1)) $ do
    current <- getArgs
    let !n=solve (read (current!!0))
    pure n
  print (foldl' (\s n -> (s+n) `mod` 1000000007) (0::Int) values)
