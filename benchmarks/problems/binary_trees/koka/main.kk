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

// Recursive make/check adapted from Koka's Benchmarks Game binarytrees.
type tree
  Node(left : tree, right : tree)
  Tip
fun make(depth : int) : div tree
  if depth > 0 then Node(make(depth - 1),make(depth - 1)) else Node(Tip,Tip)
fun check(t : tree) : div int
  match t
    Node(l,r) -> check(l)+check(r)+1
    Tip -> 0
pub fun main()
  var total := 0
  for(1,argument(1)) fn(_)
    total := total + check(make(argument(0)))
  total.println
