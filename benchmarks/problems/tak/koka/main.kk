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

pub fun tak(x : int, y : int, z : int ) : div int
  if y < x
    then tak( tak(x  -  1, y, z), tak(y  -  1, z, x), tak(z  -  1, x, y) )
    else z

pub fun main()
  var result := 0
  for(1,argument(0)) fn(_)
    result := tak(argument(1),argument(2),argument(3))
  result.println
