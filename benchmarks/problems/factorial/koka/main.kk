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

fun factorial(n : int) : div int
  if n <= 1 then 1 else n*factorial(n - 1)
pub fun main()
  var result := 0
  for(1,argument(0)) fn(_)
    result := factorial(argument(1))
  result.println
