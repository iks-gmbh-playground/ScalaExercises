/*
* To introduce the problem think to my neighbor who drives a tanker truck. The level indicator is down and
* he is worried because he does not know if he will be able to make deliveries. We put the truck on a
* horizontal ground and measured the height of the liquid in the tank.
*
* Fortunately the tank is a perfect cylinder and the vertical walls on each end are flat. The height of the
* remaining liquid is h, the diameter of the cylinder is d, the total volume is vt
* (h, d, vt are positive or null integers). You can assume that h <= d.
*
* Could you calculate the remaining volume of the liquid?
* Your function tankVol(h, d, vt) returns an integer which is the truncated result (e.g floor)
* of your float calculation.
*
* Examples:
*
* tankVol(40,120,3500) should return 1021 (calculation gives about: 1021.26992027)
* tankVol(60,120,3500) should return 1750
* tankVol(80,120,3500) should return 2478 (calculation gives about: 2478.73007973)
*/

object TankTruck {
  import math.{sin, acos, Pi}

  def tankVolH(d: Int, vt: Int)(h: Int): Int = {
    val radius = d / 2.0
    val alpha = 2.0 * acos(1.0 - h / radius)
    val length = vt / (radius * radius * Pi)
    val secant = sin(alpha/2.0) * radius
    ((radius * radius * alpha / 2 - (radius - h) * secant) * length).toInt
  }

  def tankVol(h: Int, d: Int, vt: Int): Int = tankVolH(d, vt)(h)
}

object Main extends App {
  import TankTruck._

  def th = tankVolH(7, 3848)_

  for (i <- 0 to 7) {
    println(s"i = $i -> ${th(i)}")
  }
}
