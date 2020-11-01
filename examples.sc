import com.iks.codewars.prefixDiff.PrefixDiff._

val d: Double = 2.0

val i: Int = 2
val n: Int = -1

val c1 = Constant(d)
val c2 = Constant(i)
val c3 = Constant(n)

val exp = BinaryTerm(STAR, c3, c2)
simplify(exp)

d.isValidInt
