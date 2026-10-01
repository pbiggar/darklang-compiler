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

fun place(all-bits : int64, cols : int64, left : int64, right : int64) : div int
  if cols == all-bits then 1
  else choose(all-bits,cols,left,right,all-bits.and((cols.or(left).or(right)).not),0)
fun choose(all-bits : int64, cols : int64, left : int64, right : int64, avail : int64, count : int) : div int
  if avail == 0.int64 then count
  else
    val bit = avail.and(0.int64 - avail)
    choose(all-bits,cols,left,right,avail.xor(bit),count+place(all-bits,cols.or(bit),left.or(bit).shl(1),right.or(bit).shr(1)))
pub fun main()
  place(1.int64.shl(argument(0)) - 1.int64,0.int64,0.int64,0.int64).println
