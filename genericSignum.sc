def signum[T, N >: T](n: T)(implicit num: Numeric[N]): Int = n match {
  case _ if num.lt(n, num.zero) => -1
  case _ if num.lt(num.zero, n) => 1
  case _ => 0
}

signum(10)
signum(-10)
signum(0)

signum(8.5d)
signum(-4.3d)
signum(0.0d)

signum(8.5f)
signum(-4.3f)
signum(0.0f)

signum(BigInt(8))
signum(BigInt(-4))
signum(BigInt(0))
