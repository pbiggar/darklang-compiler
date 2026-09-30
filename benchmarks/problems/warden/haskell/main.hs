-- Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
{-# LANGUAGE BangPatterns #-}
import Data.Char
import System.Environment
import Control.Monad
import Data.List (foldl')
data Token = Number !Int | Variable !Char | Symbol !Char deriving (Eq,Show)
lexProgram []=[]
lexProgram (c:cs)
  | isSpace c=lexProgram cs
  | isDigit c=let (digits,rest)=span isDigit (c:cs) in Number (read digits):lexProgram rest
  | isAlpha c=let (name,rest)=span isAlpha (c:cs) in if name `elem` ["x","y"] then Variable (name!!0):lexProgram rest else error "unknown variable"
  | c `elem` "+-*/<();"=Symbol c:lexProgram cs
  | otherwise=error "invalid token"
precedence '<'=1
precedence '+'=2
precedence '-'=2
precedence '*'=3
precedence '/'=3
precedence _=0
apply '+' a b=a+b
apply '-' a b=a-b
apply '*' a b=a*b
apply '/' a b=a `quot` b
apply '<' a b=fromEnum (a<b)
apply _ _ _=error "unknown operator"
expression x y minimum tokens = let (left,rest)=primary tokens in infixLoop left rest
  where
    primary (Number n:rest)=(n,rest)
    primary (Variable c:rest)=(if c=='x' then x else y,rest)
    primary (Symbol '(':rest)=case expression x y 0 rest of (v,Symbol ')':tail) -> (v,tail);_ -> error "expected )"
    primary _=error "invalid primary"
    infixLoop left (Symbol op:rest) | precedence op>0 && precedence op>=minimum =
      let (right,tail)=expression x y (precedence op+1) rest in infixLoop (apply op left right) tail
    infixLoop left rest=(left,rest)
evaluate tokens iteration = go 0 0 tokens
  where
    go _ !total []=total
    go index !total ts =
      let x=(iteration*17+index*13) `mod` 97+3;y=(iteration*29+index*7) `mod` 89+5
      in case expression x y 0 ts of
        (v,Symbol ';':rest) -> go (index+1) ((total+v*(index+1)) `mod` 1000000007) rest
        _ -> error "expected ;"
main = do
  args <- getArgs
  let tokens=lexProgram (concat (replicate (read (args!!0)) "x * x + y * 3 + (x + y) * (x - y) + x / 2;\n"))
  values <- forM [0..read (args!!1)-1] $ \i -> do
    let !n=evaluate tokens i
    pure n
  print (foldl' (\s n -> (s+n) `mod` 1000000007) 0 values)
