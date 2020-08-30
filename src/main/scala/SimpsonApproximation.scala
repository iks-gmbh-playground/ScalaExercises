object SimpsonApproximation extends App {

  import math.{sin, pow, Pi}

  def f(x: Double): Double = 1.5 * pow(sin(x), 3)

  def simpson(n: Int): Double = {
    val h = Pi / n
    h / 3 * ((1 until n / 2).foldLeft(0.0)((acc, i) => acc + 4 * f((2 * i - 1) * h) + 2 * f(2 * i * h)) + 4 * f((n - 1) * h))
  }

  println(simpson(72))
}
