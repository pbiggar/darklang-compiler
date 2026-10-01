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
type token
  Number(value : int)
  Variable(name : char)
  Symbol(op : char)
fun lex(source : vector<char>) : <div,exn> vector<token>
  fun digits(i : int, value : int) : <div,exn> (int,int)
    if i < source.length && source[i].is-digit then digits(i + 1,value * 10 + source[i].int - '0'.int) else (value,i)
  fun identifier(i : int) : <div,exn> int
    if i < source.length && source[i].is-alpha then identifier(i + 1) else i
  fun scan(i : int, reversed : list<token>) : <div,exn> list<token>
    if i >= source.length then reversed.reverse else
      val c = source[i]
      if c == ' ' || c == '\n' || c == '\t' || c == '\r' then scan(i + 1,reversed)
      elif c.is-digit then
        val (n,j) = digits(i,0)
        scan(j,Cons(Number(n),reversed))
      elif c.is-alpha then
        val j = identifier(i)
        if j != i + 1 || (c != 'x' && c != 'y') then throw("unknown variable")
        scan(j,Cons(Variable(c),reversed))
      else scan(i + 1,Cons(Symbol(c),reversed))
  scan(0,[]).vector
fun precedence(c : char) : <div,exn> int
  match c
    '<' -> 1
    '+' -> 2
    '-' -> 2
    '*' -> 3
    '/' -> 3
    _ -> 0
fun apply(op : char, a : int, b : int) : <div,exn> int
  match op
    '+' -> a + b
    '-' -> a - b
    '*' -> a * b
    '/' -> a / b
    '<' -> if a < b then 1 else 0
    _ -> throw("unknown operator")
fun expression(tokens : vector<token>, index : int, x : int, y : int, minimum : int) : <div,exn> (int,int)
  val (left,start) = match tokens[index]
    Number(n) -> (n,index + 1)
    Variable(c) -> (if c == 'x' then x else y,index + 1)
    Symbol('(') ->
      val (v,j) = expression(tokens,index + 1,x,y,0)
      match tokens[j]
        Symbol(')') -> (v,j + 1)
        _ -> throw("expected )")
    _ -> throw("invalid primary")
  fun infix-loop(accumulator : int, i : int) : <div,exn> (int,int)
    if i >= tokens.length then (accumulator,i) else match tokens[i]
      Symbol(op) ->
        val p = precedence(op)
        if p == 0 || p < minimum then (accumulator,i) else
          val (right,j) = expression(tokens,i + 1,x,y,p + 1)
          infix-loop(apply(op,accumulator,right),j)
      _ -> (accumulator,i)
  infix-loop(left,start)
fun evaluate(tokens : vector<token>, iteration : int) : <div,exn> int
  fun statements(i : int, index : int, sum : int) : <div,exn> int
    if i >= tokens.length then sum else
      val x = (iteration * 17 + index * 13) % 97 + 3
      val y = (iteration * 29 + index * 7) % 89 + 5
      val (v,j) = expression(tokens,i,x,y,0)
      match tokens[j]
        Symbol(';') -> statements(j + 1,index + 1,modulo(sum + v * (index + 1)))
        _ -> throw("expected ;")
  statements(0,0,0)
pub fun main()
  val tokens = lex("x * x + y * 3 + (x + y) * (x - y) + x / 2;\n".repeat(argument(0)).vector)
  var total := 0
  for(0,argument(1) - 1) fn(i)
    total := modulo(total + evaluate(tokens,i))
  total.println
