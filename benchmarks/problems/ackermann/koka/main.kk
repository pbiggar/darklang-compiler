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

fun ack(m : int, n : int) : div int
  if m == 0 then n+1
  elif n == 0 then ack(m - 1,1)
  else ack(m - 1,ack(m,n - 1))
pub fun main()
  ack(argument(0),argument(1)).println
