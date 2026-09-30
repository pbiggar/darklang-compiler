// Parameterized reference workload; provenance in benchmarks/IMPLEMENTATIONS.md.
import std/os/env
import std/num/float64
import std/core/vector
import std/num/int64

fun argument(i : int) : <ndet,exn> int
  match get-args()[i]
    Just(s) -> match s.parse-int
      Just(n) -> n
      Nothing -> throw("invalid benchmark argument")
    Nothing -> throw("missing benchmark argument")

fun hash-value(value : int64) : int64
  var h := -3750763034362895579.int64
  for(1,8) fn(_)
    h := h.xor(value.and(255.int64))*1099511628211.int64
  h
fun build(depth : int, start : int64) : div int64
  if depth==0 then hash-value(start)
  else hash-value(build(depth - 1,start)+31.int64*build(depth - 1,start+1.int64.shl(depth - 1)))
fun unsigned(h : int64) : int
  if h < 0.int64 then h.int+18446744073709551616 else h.int
pub fun main()
  var checksum := 0
  for(0,argument(1) - 1) fn(i)
    val root = build(argument(0),i.int64)
    val verified = build(argument(0),i.int64)==root
    checksum := (checksum+unsigned(root))%1000000007
    if verified then checksum := (checksum+1)%1000000007
  checksum.println
