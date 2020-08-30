object StringOperations {
  def diag1Sym(strngArr: Array[String]): Array[String] =
    strngArr.map(_.toArray).transpose.map(_.mkString)

  def rot90Clock(strngArr: Array[String]): Array[String] =
    diag1Sym(strngArr).map(_.reverse)
    //strngArr.map(_.toArray).reverse.transpose.map(_.mkString)

  def selfieAndDiag1(strngArr: Array[String]): Array[String] =
    strngArr zip diag1Sym(strngArr) map { case (a, b) => s"$a|$b"}

  def oper(f: Array[String] => Array[String], s: String): String =
    f(s.split("\n")).mkString("\n")
}
