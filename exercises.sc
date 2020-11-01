def countDown(n: Int): Unit =
  for (i <- n to 0 by -1) println(i)

countDown(5)
countDown(-4)

def product(s: String): Long = s.foldLeft(1L)(_ * _)

def product1(s: String): Long = s match {
  case "" => 1l
  case _ => s(0) * product1(s.tail)
}

"Hello".foldLeft(1L)((prod, c) => prod * c)
product("Hello")
product1("Hello")
product("")
product1("")
product("H")
product1("H")

def weirdPower(x: Double, n: Int): Double = n match {
  case _ if n == 0 => 1d
  case _ if n < 0 => 1 / weirdPower(x, -n)
  case _ if n%2 == 0 =>
    val y = weirdPower(x, n/2)
    y * y
  case _ => x * weirdPower(x, n-1)
}

weirdPower(2, 3)
weirdPower(2, 0)
weirdPower(8, -2)
weirdPower(2, 4)
