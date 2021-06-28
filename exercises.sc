import scala.annotation.tailrec

def measureTime[A, B](f: A => B)(a: A): (B, Long) = {
  val start = System.nanoTime()
  val result = f(a)
  val end = System.nanoTime()
  (result, end - start)
}

@tailrec
def fibonacci(n: Int, n0: BigInt = 0, n1: BigInt = 1): BigInt = n match {
  case 0 => n0
  case 1 => n1
  case -1 => n1
  case _ if n < 0 => fibonacci(n + 1, n1, n0 - n1)
  case _ => fibonacci(n - 1, n1, n0 + n1)
}

@tailrec
def fib2(n: Int,
         b: BigInt = 0, a: BigInt = 1,
         p: BigInt = 0, q: BigInt = 1): BigInt = n match {
  case 0 => b
  case _ if n % 2 == 0 => fib2(n / 2, b, a, p*p + q*q, q*q + 2*p*q)
  case _ => fib2(n - 1, b*p + a*q , b*q + a*q + a*p, p, q)
}

def fib(n: Int): BigInt =
  if (n < 0 && n % 2 == 0) -fib2(-n)
  else fib2(math.abs(n))

for (i <- 0 to 10) println(s"$i: ${fibonacci(i)} <-> ${fibonacci(-i)}")
for (i <- 0 to 10) println(s"$i: ${fib(i)} <-> ${fib(-i)}")

val (result2, time2) = measureTime((n: Int) => fib(n))(2000000)
result2.toString.length
s"${time2/1000000d} milliseconds"

val (result1, time1) = measureTime((n: Int) => fibonacci(n))(2000000)
result1.toString.length
s"${time1/1000000d} milliseconds"

