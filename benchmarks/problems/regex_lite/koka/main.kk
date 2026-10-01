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
type atom
  Literal(c : char)
  Any
  Range(low : char, high : char)
type term
  Term(atom : atom, minimum : int, maximum : int)
fun parse-branch(source : vector<char>, i : int) : <div,exn> list<term>
  if i >= source.length then [] else
    val (atom,next) = match source[i]
      '[' -> (Range(source[i + 1],source[i + 3]),i + 5)
      '.' -> (Any,i + 1)
      c -> (Literal(c),i + 1)
    val (low,high,j) = if next >= source.length then (1,1,next) else match source[next]
      '*' -> (0,2147483647,next + 1)
      '+' -> (1,2147483647,next + 1)
      '?' -> (0,1,next + 1)
      _ -> (1,1,next)
    Cons(Term(atom,low,high),parse-branch(source,j))
fun accepts(atom : atom, c : char) : <div,exn> bool
  match atom
    Literal(a) -> a == c
    Any -> True
    Range(a,b) -> c >= a && c <= b
fun matches(terms : list<term>, text : vector<char>, position : int) : <div,exn> bool
  match terms
    Nil -> True
    Cons(t,rest) ->
      fun consume(count : int) : <div,exn> int
        if count < t.maximum && position + count < text.length && accepts(t.atom,text[position + count]) then consume(count + 1) else count
      val count = consume(0)
      fun try-count(i : int) : <div,exn> bool
        i <= count && (matches(rest,text,position + i) || try-count(i + 1))
      try-count(t.minimum)
pub fun main()
  val pattern = "dark[a-z]*lang|compiler[0-9]+|ab[0-9]?".split("|").map(fn(s) parse-branch(s.vector,0))
  val text = "darklang darkxxlang compiler42 compiler ab ab7 nope DARKlang compilerx\n".repeat(argument(0)).vector
  var total := 0
  for(1,argument(1)) fn(_)
    val contribution = scan(text,pattern)
    total := modulo(total + contribution)
  total.println
fun scan(text : vector<char>, pattern : list<list<term>>) : <div,exn> int
  var count := 0
  var checksum := 0
  for(0,text.length - 1) fn(i)
    if pattern.any(fn(terms) matches(terms,text,i)) then
      checksum := modulo(checksum + (i + 1) * (count + 3))
      count := count + 1
  modulo(count * 1000003 + checksum)
