// Parameterized reference workload; provenance in benchmarks/IMPLEMENTATIONS.md.
import std/os/env
import std/num/float64
import std/core/vector

fun argument(i : int) : <ndet,exn> int
  match get-args()[i]
    Just(s) -> match s.parse-int
      Just(n) -> n
      Nothing -> throw("invalid benchmark argument")
    Nothing -> throw("missing benchmark argument")

fun steps(n : int, s : int) : div int
  if n == 1 then s
  else steps(if n%2==0 then n / 2 else 3*n+1,s+1)
pub fun main()
  var total := 0
  for(1,argument(0)) fn(i)
    total := total + steps(i,0)
  total.println
