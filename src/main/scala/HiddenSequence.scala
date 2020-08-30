object HiddenSequence extends App {
  // Given u0 = 1, u1 = 2 and the relation 6unun+1-5unun+2+un+1un+2 = 0 calculate un for any integer n >= 0.

  def fcn(n: Int): BigInt = BigInt(1) << n
  
  def fcnn = BigInt(1) << _

  for (n <- 0 to 15) println(s"fcn($n) == ${fcn(n)}")
}
