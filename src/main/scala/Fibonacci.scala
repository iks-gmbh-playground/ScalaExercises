object Fibonacci extends App {
  def fibonacci(n: Int): BigInt =
    if (n < 2) 1 else (2 to n).foldLeft((BigInt(1), BigInt(1)))((z, _) => (z._2, z._1 + z._2))._2

  def fibonacciLoop(n: Int): BigInt = {
    if (n < 2) 1 else {
      var n0 = BigInt(1)
      var n1 = BigInt(1)
      for (_ <- 2 to n) {
        val sum  = n0 + n1
        n0 = n1
        n1 = sum
      }
      n1
    }


  }

  println(s"fibonacci(0) is ${fibonacciLoop(0)}")
  println(s"fibonacci(1) is ${fibonacciLoop(1)}")
  println(s"fibonacci(2) is ${fibonacciLoop(2)}")
  println(s"fibonacci(3) is ${fibonacciLoop(3)}")
  println(s"fibonacci(4) is ${fibonacciLoop(4)}")
  println(s"fibonacci(5) is ${fibonacciLoop(5)}")
  println(s"fibonacci(6) is ${fibonacciLoop(6)}")
  println(s"fibonacci(7) is ${fibonacciLoop(7)}")
  println(s"fibonacci(8) is ${fibonacciLoop(8)}")
  println(s"fibonacci(250) is ${fibonacci(250)}")
  println(s"fibonacciLoop(250) is ${fibonacciLoop(250)}")
}
