object PascalTriangle extends App {

  def pascal(p: Int): List[List[BigInt]] = {
    @scala.annotation.tailrec
    def loop(r: Int, triangle: List[List[BigInt]] = List(List(BigInt(1)))): List[List[BigInt]] =
      if (r > p)
        triangle
      else {
        val row = (for (c <- 2 to r) yield if (c == r) BigInt(1) else triangle.head(c-2) + triangle.head(c-1)).toList
        loop(r+1, (BigInt(1) :: row) :: triangle)
      }
    loop(2).reverse
  }

  def myPascal(p: Int): List[List[BigInt]] =
    List.iterate(List(BigInt(1)), p)(line => (BigInt(0) :: (line :+ BigInt(0))).sliding(2).map(_.sum).toList)

  println(pascal(5))
  println(myPascal(5))
}