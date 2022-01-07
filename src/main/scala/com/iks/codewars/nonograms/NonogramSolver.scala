package com.iks.codewars.nonograms

import scala.annotation.tailrec
import scala.collection._

object NonogramSolver {

  type Clue = List[Int]
  type Clues = List[Clue]

  implicit class nonoGramString(val str: String) {
    def isPossiblyValid(clue: Clue, size: Int): Boolean =
      if (str.length == size) isValid(clue, size)
      else if (str.length > size) false
      else if (str.isEmpty) true
      else if (clue.sum == 0) str.matches(s"0*")
      else if (clue.size == 1)
        str.matches(s"0*1{${clue.head}}0+") || str.matches(s"0*1{0,${clue.head}}")
      else
        str.matches(s"0*1{0,${clue.head}}") &&
          (size - str.takeWhile(_ == '0').length >= clue.sum + clue.size - 1) ||
          str.matches(s"0*1{${clue.head}}0[0,1]*") &&
            str.dropWhile(_ == '0').drop(clue.head).isPossiblyValid(clue.tail, size - str.segmentLength(_ == '0') - clue.head)

    def isValid(clue: Clue, size: Int): Boolean =
      if (str.length != size) false
      else if (clue.sum == 0) str.matches(s"0{$size}")
      else if (clue.size == 1) str.matches(s"0*1{${clue.head}}0*")
      else if (clue.size > 1 && str.matches(s"0*1{${clue.head}}0[0,1]*"))
        str.dropWhile(_ == '0').drop(clue.head).isValid(clue.tail, size - str.segmentLength(_ == '0') - clue.head)
      else false
  }

  case class Board(rows: IndexedSeq[String]) {
    // assert(rows.forall(_.length == rows.size), s"rows: $rows")

    val size: Int = if (rows.isEmpty) 0 else rows.head.length

    lazy val columns: IndexedSeq[String] =
      for (j <- 0 until size) yield
        (for (i <- rows.indices) yield rows(i)(j)).mkString

    override def toString: String = (for (i <- rows.indices) yield
      (for (j <- rows(i).indices) yield rows(i)(j)).mkString("(", ", ", ")")).mkString("(", "\n ", ")")

    def isValid(rowClues: Clues, columnClues: Clues): Boolean =
      (rows zip rowClues forall { case (row, clue) => row.isValid(clue, rows.size) }) &&
        (columns zip columnClues forall { case (column, clue) => column.isValid(clue, rows.size) })

    def isPossiblyValid(rowClues: Clues, columnClues: Clues): Boolean =
      (rows zip rowClues forall { case (row, clue) => row.isPossiblyValid(clue, rows.head.length) }) &&
        (columns zip columnClues forall { case (column, clue) => column.isPossiblyValid(clue, rows.head.length) })
  }

  def validStrings(clue: Clue, size: Int,
                   prefix: String = "", acc: IndexedSeq[String] = IndexedSeq.empty): IndexedSeq[String] =
    if (clue.sum == 0)
      (prefix + "0" * size) +: acc
    else if (clue.size == 1)
      if (size - clue.head <= 0)
        (prefix + "1" * clue.head) +: acc
      else
        (prefix + "1" * clue.head + "0" * (size - clue.head)) +: validStrings(clue, size - 1, prefix + "0", acc)
    else if (clue.size + clue.sum == size + 1)
      (prefix + (for (c <- clue) yield "1" * c).mkString("0")) +: acc
    else
      (0 to size - clue.sum - clue.size + 1).foldLeft(acc)((ll, i) => {
        val prefixNew = prefix + "0" * i + "1" * clue.head + "0"
        validStrings(clue.tail, size - i - clue.head - 1, prefixNew, ll)
      })

  def solve(rowClues: Clues, columnClues: Clues): Option[Board] = {

    def findValidBoard(allValidStrings: List[IndexedSeq[String]],
                       rowStrings: IndexedSeq[String]): Option[Board] = {
      @tailrec
      def innerLoop(validStrings: IndexedSeq[String]): Option[Board] =
        if (validStrings.isEmpty)
          None
        else {
          val newRowStrings = rowStrings :+ validStrings.head
          if (Board(newRowStrings).isPossiblyValid(rowClues, columnClues)) {
            val maybeBoard = findValidBoard(allValidStrings.tail, newRowStrings)
            if (maybeBoard.isEmpty)
              innerLoop(validStrings.tail)
            else
              maybeBoard
          } else innerLoop(validStrings.tail)
        }

      if (allValidStrings.isEmpty) {
        val board = Board(rowStrings)
        if (board.isValid(rowClues, columnClues)) Some(board) else None
      } else
        innerLoop(allValidStrings.head)
    }

    val allValidStrings = for (c <- rowClues) yield validStrings(c, columnClues.size)
    val counter = allValidStrings.foldLeft(1L)((c, vs) => c * vs.size)
    println(s"nonogramSolver started for $counter possible Boards.")
    findValidBoard(allValidStrings, IndexedSeq.empty[String])
  }
}
