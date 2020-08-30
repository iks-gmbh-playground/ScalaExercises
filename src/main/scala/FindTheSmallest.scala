object FindTheSmallest {
  def moveDigit(digits: String, from: Int, to: Int): Array[Long] = {
    val temp = digits.take(from) + digits.drop(from + 1)
    Array((temp.take(to) + digits(from) + temp.drop(to)).toLong, from, to)
  }

  def smallest(n: Long): Array[Long] = {
    val digits = n.toString
    (for {
      from <- 0 until digits.length
      to <- 0 until digits.length
    } yield moveDigit(digits, from, to)).minBy { case Array(n, f, t) => (n, f, t) }
  }
}
