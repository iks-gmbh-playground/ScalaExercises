import com.iks.codewars.prefixDiff.PrefixDiff._

validatePrefixTerm("x")
validatePrefixTerm("123")
validatePrefixTerm("(cos x)")
validatePrefixTerm("(cos (+ x 1))")
validatePrefixTerm("(sin (+ 2 (cos (+ x 1))))")
validatePrefixTerm("(sin (+ 2 )(cos (+ x 1))))")
validatePrefixTerm("(sin (+ 2 )(cos (+ y 1))))")
validatePrefixTerm("(sin (+ 2 )(cot (+ x 1))))")
validatePrefixTerm("(sin (+ 2 (cos (+ x 1)))")

val str = "(+ (* 1 x) (* 2 (+ x 1)))"
validatePrefixTerm(str)
val exp = makeExpression(str)
println(simplify(exp).toInfix)
val expDiff = differentiate(exp)
println(simplify(expDiff).toInfix)

val exp1 = makeExpression("(+ 1 x)")
simplify(differentiate(exp1)).toInfix

val exp2 = makeExpression("(* 1 x)")
simplify(differentiate(exp2)).toInfix

val str1 = "(^ x 3)"
val prefixExpression(expr) = str1
val (op, args) =extractOperator(expr)
unaryOperator.matches(op)
binaryOperator.matches(op)

val exp3 = makeExpression("(^ x 3)") // (* 3 (^ x 2))
simplify(differentiate(exp3)).toString

val exp4 = makeExpression("(cos x)") // (* -1 (sin x))
simplify(differentiate(exp4)).toString

val exp5 = makeExpression("(tan x)")
simplify(differentiate(exp5)).toString

val exp6 = makeExpression("(exp x)")
simplify(differentiate(exp6)).toString

val exp7 = makeExpression("(ln x)")
simplify(differentiate(exp7)).toString

val exp8 = makeExpression("(/ x 2)")
simplify(differentiate(exp8)).toString

val exp9 = makeExpression("(/ 2 (+ 1 x))")
differentiate(exp9).toString
simplify(differentiate(exp9)).toString

val exp10 = makeExpression("(tan (* 2 x))")
simplify(differentiate(exp10)).toString

val exp11 = makeExpression("(^ (sin x) 3)")
val possibleResults11 = List("(* (* 3 (^ (sin x) 2)) (cos x))", "(* (cos x) (* 3 (^ (sin x) 2)))", "(* (* (^ (sin x) 2) 3) (cos x))", "(* (cos x) (* (^ (sin x) 2) 3))")
possibleResults11.foreach(println)
val result11 = simplify(differentiate(exp11)).toString
possibleResults11.contains(result11)
