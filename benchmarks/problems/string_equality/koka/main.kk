// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import std/os/env
import std/num/float64
import std/core/vector
fun argument(i : int) : <ndet,exn> int
  match get-args()[i]
    Just(s) -> match s.parse-int
      Just(n) -> n
      Nothing -> throw("invalid argument")
    Nothing -> throw("missing argument")
val modulus = 1000000007
fun modulo(n : int) : <div,exn> int
  ((n % modulus) + modulus) % modulus
pub fun main()
  val rounds = argument(0)
  val token = match get-args()[1]
    Just(s) -> s
    Nothing -> throw("missing token")
  val middle = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".repeat(2)
  val short = "ab" ++ token ++ "cd"
  val long = "prefix:" ++ token ++ ":" ++ middle ++ ":suffix"
  val short-cases = [short,["a","b",token,"cd"].join,short ++ "x","xb" ++ token ++ "cd","ab" ++ token ++ "ce"]
  val long-cases = [long,["pre","fix:",token,":",middle,":suffix"].join,long ++ "!","xrefix:" ++ token ++ ":" ++ middle ++ ":suffix","prefix:" ++ token ++ ":" ++ middle ++ ":suffiy"]
  var total := 0
  for(1,rounds) fn(_)
    short-cases.foreach-indexed fn(i,other)
      if short == other then total := total + (2.pow(i))
    long-cases.foreach-indexed fn(i,other)
      if long == other then total := total + 32 * (2.pow(i))
  total.println
