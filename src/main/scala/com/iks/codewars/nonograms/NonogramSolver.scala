package com.iks.codewars.nonograms

import scala.annotation.tailrec

object NonogramSolver {

  type Clue = List[Int]
  type Clues = List[Clue]

  implicit class NonogramString(val str: String) {
    def isPossiblyValidFor(clue: Clue, size: Int): Boolean =
      if (str.length == size) isValidFor(clue, size)
      else if (str.length > size) false
      else if (str.isEmpty) true
      else if (clue.sum == 0) str.matches(s"0*")
      else if (clue.size == 1)
        str.matches(s"0*1{${clue.head}}0+") || str.matches(s"0*1{0,${clue.head}}")
      else
        str.matches(s"0*1{0,${clue.head}}") &&
          (size - str.takeWhile(_ == '0').length >= clue.sum + clue.size - 1) ||
          str.matches(s"0*1{${clue.head}}0[0,1]*") &&
            str.dropWhile(_ == '0').drop(clue.head).isPossiblyValidFor(clue.tail, size - str.segmentLength(_ == '0') - clue.head)

    def isValidFor(clue: Clue, size: Int): Boolean =
      if (str.length != size) false
      else if (clue.sum == 0) str.matches(s"0{$size}")
      else if (clue.size == 1) str.matches(s"0*1{${clue.head}}0*")
      else if (clue.size > 1 && str.matches(s"0*1{${clue.head}}0[0,1]*"))
        str.dropWhile(_ == '0').drop(clue.head).isValidFor(clue.tail, size - str.segmentLength(_ == '0') - clue.head)
      else false
  }

  case class Board(rows: List[String]) {
    val size: Int = if (rows.isEmpty) 0 else rows.head.length

    assert(rows.forall(_.length == size))

    val columns: List[String] =
      (for (j <- 0 until size) yield
        (for (row <- rows) yield row(j)).mkString).toList

    override def toString: String = (for (row <- rows) yield
      row.toList.mkString("(", ", ", ")")).mkString("(", "\n ", ")")

    def isValid(rowClues: Clues, columnClues: Clues): Boolean =
      (columns zip columnClues forall { case (column, clue) => column.isValidFor(clue, rows.size) }) &&
        (rows zip rowClues forall { case (row, clue) => row.isValidFor(clue, rows.size) })

    def isPossiblyValid(rowClues: Clues, columnClues: Clues): Boolean =
      (columns zip columnClues forall { case (column, clue) => column.isPossiblyValidFor(clue, rows.head.length) }) &&
        (rows zip rowClues forall { case (row, clue) => row.isPossiblyValidFor(clue, rows.head.length) })
  }

  def createAllValidStringsFor(clue: Clue, size: Int,
                               prefix: String = "", acc: List[String] = List.empty): List[String] =
    if (clue.sum == 0)
      (prefix + "0" * size) :: acc
    else if (clue.size == 1)
      if (size - clue.head <= 0)
        (prefix + "1" * clue.head) :: acc
      else
        (prefix + "1" * clue.head + "0" * (size - clue.head)) :: createAllValidStringsFor(clue, size - 1, prefix + "0", acc)
    else if (clue.size + clue.sum == size + 1)
      (prefix + (for (c <- clue) yield "1" * c).mkString("0")) :: acc
    else
      (0 to size - clue.sum - clue.size + 1).foldLeft(acc)((ll, i) => {
        val prefixNew = prefix + "0" * i + "1" * clue.head + "0"
        createAllValidStringsFor(clue.tail, size - i - clue.head - 1, prefixNew, ll)
      })

  def solve(rowClues: Clues, columnClues: Clues): Option[Board] = {

    def collectRowsToValidBoard(allValidStringsForAllRows: List[List[String]],
                       collectedRows: List[String] = List.empty): Option[Board] = {
      @tailrec
      def appendNextValidRow(validStringsForNextRow: List[String]): Option[Board] =
        validStringsForNextRow match {
          case List() => None
          case validRowString :: moreValidRowStrings =>
            if (Board(collectedRows :+ validRowString).isPossiblyValid(rowClues, columnClues))
              collectRowsToValidBoard(allValidStringsForAllRows.tail, collectedRows :+ validRowString) match {
                case None => appendNextValidRow(moreValidRowStrings)
                case success => success
              }
            else appendNextValidRow(moreValidRowStrings)
        }

      if (allValidStringsForAllRows.isEmpty) Some(Board(collectedRows))
      else appendNextValidRow(allValidStringsForAllRows.head)
    }

    val allValidStringsForAllRows = for (rowClue <- rowClues) yield createAllValidStringsFor(rowClue, columnClues.size)
    val counter = allValidStringsForAllRows.foldLeft(BigInt("1"))((c, vs) => c * vs.size)
    println(s"nonogramSolver started for $counter possible Boards.")
    collectRowsToValidBoard(allValidStringsForAllRows)
  }
}
