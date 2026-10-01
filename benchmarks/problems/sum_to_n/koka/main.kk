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

fun sum-to(n : int, s : int) : div int
  if n <= 0 then s else sum-to(n - 1,s+n)
pub fun main()
  var result := 0
  for(1,argument(0)) fn(_)
    result := sum-to(argument(1),0)
  result.println
