object BallUpwards extends App {
  // You throw a ball vertically upwards with an initial speed v (in km per hour).
  // The height h of the ball at each time t is given by h = v*t - 0.5*g*t*t
  // where g is Earth's gravity (g ~ 9.81 m/s**2).
  // A device is recording at every tenth of second the height of the ball.
  // For example with v = 15 km/h the device gets something of the following form:
  // (0, 0.0), (1, 0.367...), (2, 0.637...), (3, 0.808...), (4, 0.881..) ...
  // where the first number is the time in tenth of second and the second number the height in meter.
  //
  // Write a function max_ball with parameter v (in km per hour)
  // that returns the time in tenth of second of the maximum height recorded by the device.
  def maxBall(v0: Int): Int = {
    def height(t: Int): Double = v0 * 1.0 / 36 * t - 9.81 * t * t / 200

    @scala.annotation.tailrec
    def loop(h: Double = 0.0, t: Int = 0): Int =
      if (height(t + 1) < h) t else loop(height(t + 1), t + 1)

    loop()
  }

  println(s"v0=15 results in max.height of ${maxBall(15)}")
  println(s"v0=25 results in max.height of ${maxBall(25)}")
}
