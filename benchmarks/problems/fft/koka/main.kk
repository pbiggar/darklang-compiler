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
type complex
  Complex(re : float64, im : float64)
fun fft(xs : vector<complex>) : <div,exn> vector<complex>
  val n = xs.length
  if n <= 1 then xs else
    val even = fft(vector-init(n / 2, fn(i) xs[i * 2]))
    val odd = fft(vector-init(n / 2, fn(i) xs[i * 2 + 1]))
    val twiddle = vector-init(n / 2, fn(i)
      val angle = -6.283185307179586 * i.float64 / n.float64
      val c = angle.cos
      val s = angle.sin
      val z = odd[i]
      Complex(c * z.re - s * z.im, c * z.im + s * z.re)
    )
    vector-init(n, fn(i)
      val j = i % (n / 2)
      val a = even[j]
      val b = twiddle[j]
      if i < n / 2 then Complex(a.re + b.re,a.im + b.im) else Complex(a.re - b.re,a.im - b.im)
    )
pub fun main()
  val n = argument(0)
  val xs = vector-init(n, fn(i)
    val x = i.float64
    Complex((x * 0.017).sin + (x * 0.031).cos,(x * 0.013).cos - (x * 0.007).sin)
  )
  var total := 0
  for(1,argument(1)) fn(_)
    val result = fft(xs)
    result.foreach-indexed fn(i,z)
      total := modulo(total + ((z.re * 3.0 + z.im * 5.0) * 1000000.0).truncate.int * (i + 1))
  total.println
