package com.iks.codewars.prefixDiff

import scala.annotation.tailrec
import scala.util.matching.Regex

object PrefixDiff {

  val SIN = "sin"
  val COS = "cos"
  val TAN = "tan"
  val LN = "ln"
  val EXP = "exp"
  val PLUS = "+"
  val MINUS = "-"
  val STAR = "*"
  val SLASH = "/"
  val HAT = "^"
  val prefixExpression: Regex = """\((.*)\)""".r
  val unaryOperator: Regex = """sin|cos|tan|exp|ln""".r
  val binaryOperator: Regex = """[+\-*/^]""".r
  val number: Regex = """(-?[0-9]+)(\.[0-9]+)?""".r
  val startsWithNumber: Regex = """-?[0-9]+(\.[0-9]+)?(.*)""".r
  val startsWithOperator: Regex = """(sin|cos|tan|exp|ln|\+|-|\*|/|\^) (.*)""".r

  def extractOperator(str: String): (String, String) = {
    val index = str.indexOf(' ')
    if (index < 0) ("", str)
    else (str.slice(0, index), str.substring(index).trim)
  }

  def extractParameters(str: String): (String, String) =
    if (str.trim.head == '(')
      extractParametersInParentheses(str)
    else
      (str.slice(0, str.indexOf(' ')), str.substring(str.indexOf(' ')).trim)

  /*
   *  if str does not start with a '(' extractParametersInParentheses returns ("", str)
   *  if extractParametersInParentheses does not find a matching ')' it returns ("", str)
   *  if str is "(x (xx) xx) (yy(((yyy)" extractParametersInParentheses returns a pair ("(x (xx) xx)", "(yy(((yyy)")
   *  if str is "(xx (xx) x) y" extractParametersInParentheses returns a pair ("(xx (xx) x)", "y")
   */
  def extractParametersInParentheses(str: String): (String, String) = {
    val start = str.indexOf('(')
    if (start < 0)
      ("", str)
    else {
      var openClose = 1
      var index = start
      while (index < str.length && openClose > 0) {
        index += 1
        str(index) match {
          case '(' => openClose += 1
          case ')' => openClose -= 1
          case _ =>
        }
      }
      if (openClose == 0) (str.substring(start, index + 1), str.substring(index + 1).trim)
      else ("", str)
    }
  }

  def validatePrefixTerm(str: String): Boolean = {
    @tailrec
    def loop(str: String, balancedParentheses: Int = 0): Boolean = {
      str match {
        case "" => if (balancedParentheses == 0) true else false
        case "x" => if (balancedParentheses == 0) true else false
        case s"($tail" => loop(tail.trim, balancedParentheses + 1)
        case s")$_" if balancedParentheses < 1 => false
        case s")$tail" => loop(tail.trim, balancedParentheses - 1)
        case startsWithOperator(_, rest) => loop(rest.trim, balancedParentheses)
        case startsWithNumber(_, rest) => loop(rest.trim, balancedParentheses)
        case s"x$tail" => loop(tail.trim, balancedParentheses)
        case _ => false
      }
    }

    loop(str)
  }

  def makeExpression(str: String): Expression = {
    str match {
      case "x" => Variable(str)
      case number(_, _) => Constant(str.toInt)
      case prefixExpression(expr) =>
        extractOperator(expr) match {
          case (op, arg) if unaryOperator.matches(op) => UnaryTerm(op, makeExpression(arg))
          case (op, args) if binaryOperator.matches(op) =>
            val (arg1, arg2) = extractParameters(args)
            BinaryTerm(op, makeExpression(arg1), makeExpression(arg2))
        }
      case err => throw new IllegalArgumentException(s"$err cannot be parsed")
    }
  }

  def differentiate(expression: Expression): Expression =
    expression match {
      case Constant(_) => Constant(0)
      case Variable(_) => Constant(1)
      case UnaryTerm(op, a) => op match {
        case SIN => BinaryTerm(STAR, differentiate(a), UnaryTerm(COS, a))
        case COS => BinaryTerm(STAR, differentiate(a), BinaryTerm(STAR, Constant(-1), UnaryTerm(SIN, a)))
        case TAN => BinaryTerm(STAR, differentiate(a), BinaryTerm(PLUS, Constant(1), BinaryTerm(HAT, UnaryTerm(TAN, a), Constant(2))))
        case LN => BinaryTerm(SLASH, differentiate(a), a)
        case EXP => BinaryTerm(STAR, differentiate(a), UnaryTerm(EXP, a))
      }
      case BinaryTerm(HAT, a, Constant(n)) => BinaryTerm(STAR, BinaryTerm(STAR, Constant(n), BinaryTerm(HAT, a, Constant(n - 1))), differentiate(a))
      case BinaryTerm(op, a, b) => op match {
        case PLUS => BinaryTerm(PLUS, differentiate(a), differentiate(b))
        case MINUS => BinaryTerm(MINUS, differentiate(a), differentiate(b))
        case STAR => BinaryTerm(PLUS, BinaryTerm(STAR, differentiate(a), b), BinaryTerm(STAR, a, differentiate(b)))
        case SLASH => BinaryTerm(SLASH, BinaryTerm(MINUS, BinaryTerm(STAR, differentiate(a), b), BinaryTerm(STAR, a, differentiate(b))), BinaryTerm(HAT, b, Constant(2)))
      }
    }

  def simplify(expression: Expression): Expression =
    expression match {
      case Constant(c) => Constant(c)
      case Variable(x) => Variable(x)
      case UnaryTerm(op, exp) => UnaryTerm(op, simplify(exp))
      case BinaryTerm(HAT, _, Constant(0)) => Constant(1)
      case BinaryTerm(op, a, b) =>
        val sa = fullySimplify(a)
        val sb = fullySimplify(b)
        (op, sa, sb) match {
          case (HAT, exp, Constant(1)) => exp
          case (HAT, Constant(x), Constant(y)) => Constant(math.pow(x, y).toInt)
          case (HAT, sa, sb) => BinaryTerm(HAT, sa, sb)

          case (PLUS, sa, Constant(0)) => sa
          case (PLUS, Constant(0), sb) => sb
          case (PLUS, Constant(x), Constant(y)) => Constant(x + y)
          case (PLUS, sa, sb) => BinaryTerm(PLUS, sa, sb)

          case (MINUS, sa, Constant(0)) => sa
          case (MINUS, Constant(0), sb) => BinaryTerm(STAR, Constant(-1), sb)
          case (MINUS, Constant(x), Constant(y)) => Constant(x - y)
          case (MINUS, sa, sb) => BinaryTerm(MINUS, sa, sb)

          case (STAR, _, Constant(0)) => Constant(0)
          case (STAR, Constant(0), _) => Constant(0)
          case (STAR, sa, Constant(1)) => sa
          case (STAR, Constant(1), sb) => sb
          case (STAR, Constant(x), Constant(y)) => Constant(x * y)
          case (STAR, sa, Constant(c)) => BinaryTerm(STAR, Constant(c), sa)
          case (STAR, sa, sb) => BinaryTerm(STAR, sa, sb)

          case (SLASH, sa, Constant(1)) => sa
          case (SLASH, Constant(x), Constant(y)) => Constant(x / y)
          case (SLASH, sa, sb) => BinaryTerm(SLASH, sa, sb)

          case (op, sa, sb) => BinaryTerm(op, sa, sb)
        }
    }

  @tailrec
  def fullySimplify(exp: Expression): Expression = {
    val simpleExp = simplify(exp)
    if (simpleExp != exp) fullySimplify(simpleExp) else simpleExp
  }

  abstract sealed class Expression() {
    def toInfix: String
  }

  case class Constant(c: Double) extends Expression {
    val cStr: String = if (c.isWhole) c.intValue.toString else c.toString

    override def toString: String = s"$cStr"

    def toInfix: String = s"$cStr"
  }

  case class Variable(variable: String) extends Expression {
    override def toString: String = variable

    def toInfix: String = variable
  }

  case class UnaryTerm(op: String, arg: Expression) extends Expression {
    override def toString: String = s"($op $arg)"

    def toInfix: String = s"$op($arg)"
  }

  case class BinaryTerm(op: String, arg1: Expression, arg2: Expression) extends Expression {
    override def toString: String = s"($op $arg1 $arg2)"

    def toInfix: String = s"(${arg1.toInfix} $op ${arg2.toInfix})"
  }

}
