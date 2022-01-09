package com.iks.codewars.nonograms

import com.iks.codewars.nonograms.NonogramSolver.{Board, Clues}

object NonogramRunner extends App {
  val rc5 = List(List(1), List(2), List(3), List(2, 1), List(4))
  val cc5 = List(List(1, 1), List(4), List(1, 1, 1), List(3), List(1))

  val rc10 = List(List(4), List(1, 3), List(2, 1), List(5), List(6), List(6), List(3, 2), List(5, 3), List(1, 1, 2), List(5))
  val cc10 = List(List(7), List(5, 1), List(9), List(4, 1, 1), List(3, 1, 1), List(2), List(2), List(2, 1), List(3, 3), List(1, 3))

  val rc15 = List(List(5, 1, 1), List(3, 2, 5), List(3, 2, 4), List(4, 1, 6), List(5, 5), List(1, 1, 6), List(1, 3), List(8), List(1, 2, 2), List(3), List(8), List(9), List(7, 2), List(1, 3, 2), List(1, 1, 1, 3))
  val cc15 = List(List(5, 3, 2), List(5, 4), List(5, 4, 1), List(2, 2, 3), List(1, 2, 4), List(4, 4), List(2, 5), List(2, 3), List(5, 2), List(5), List(7), List(6, 1), List(5, 1, 1), List(4, 1, 2, 3), List(1, 1, 2, 3))

  val rc20 = List(List(3, 3, 4), List(1, 1, 4), List(1, 1, 3, 1), List(2, 1, 3), List(3, 1, 1, 4), List(1, 1), List(1, 3, 3, 3, 2), List(6, 4, 3), List(6, 8, 3), List(19), List(2, 10, 3), List(2, 9, 4), List(1, 5, 3), List(1, 1, 1, 3), List(7, 3), List(1, 1, 10), List(1, 3, 6), List(3), List(1, 4, 2, 2), List(1, 5, 3, 2))
  val cc20 = List(List(5, 6, 2, 2), List(1, 2, 5), List(3, 1, 3), List(4), List(3, 4), List(3, 8, 1, 1), List(5, 3, 2), List(1, 1, 4, 2), List(1, 1, 4, 2), List(1, 7, 2), List(5, 3), List(6, 3, 1), List(7, 3, 2), List(10, 2), List(2, 5, 3), List(2, 1, 3), List(1, 3, 3, 2), List(1, 1, 1, 11), List(1, 15), List(1, 3, 9))

  val rc25 = List(List(2, 12, 3, 2), List(1, 6, 1, 3, 2, 2), List(5, 2, 3, 1), List(5, 5, 3), List(2, 9, 3), List(2, 7, 3, 2), List(7, 1, 1, 1), List(7, 2, 3), List(3, 2, 1, 3), List(5, 3, 2), List(1, 6, 1, 3, 1, 1), List(2, 14, 1, 3), List(2, 3, 1, 4), List(3, 5), List(2, 1, 5), List(6, 1, 6), List(4, 8, 5, 1), List(3, 11, 1), List(3, 8, 1), List(1, 8, 1), List(4, 4, 1), List(3, 6, 6, 4), List(3, 6, 3), List(3, 2, 4), List(4, 3))
  val cc25 = List(List(6, 2, 1, 1, 1), List(1, 4, 3, 2, 1, 1), List(2, 4, 1, 2), List(1, 2, 3, 1), List(2, 3, 3, 1, 1), List(2, 3, 6, 3), List(2, 11, 3), List(15, 2), List(8, 3, 2, 2), List(8, 1, 7), List(1, 6, 1, 7), List(5, 3, 1, 7), List(1, 1, 2, 3, 5), List(2, 1, 6), List(3, 4, 7), List(3, 3, 8), List(1, 1, 3, 6), List(3, 2, 5), List(2, 1, 2, 3, 4), List(5, 5, 4), List(6, 5, 1), List(1, 2, 11), List(3, 5, 2), List(2, 10, 2), List(3, 4, 2))

  val rc25a = List(List(1, 1, 6), List(1, 6), List(2, 2, 1, 9), List(4, 2, 5, 2), List(2, 3, 4, 1), List(4, 3), List(6, 1, 1), List(6, 1, 1, 2), List(6, 1, 2), List(7, 3, 3), List(7, 1, 1, 1, 3), List(10, 1, 4, 4), List(6, 1, 6, 3, 3), List(1, 5, 1, 3), List(2, 3, 6, 3), List(5, 1, 4, 5), List(6, 3, 3, 4), List(4, 6, 6), List(5, 15), List(4, 6, 3), List(8, 1, 1), List(1, 6, 2), List(1, 6, 7), List(2, 6, 5), List(2, 7, 4))
  val cc25a = List(List(1, 5, 1, 1, 3, 4), List(1, 7, 6, 2), List(11, 5), List(11, 6), List(8, 3, 2), List(12, 1), List(3, 1), List(1, 3, 2, 3, 1), List(3, 1, 5), List(2, 1, 5), List(1, 3, 10), List(3, 9), List(4, 8), List(3, 11, 2), List(2, 1, 8), List(1, 3, 7), List(3, 4, 1), List(4, 1, 3, 1), List(8, 2, 3, 1), List(6, 1, 4, 1), List(5, 1, 9), List(5, 4, 5, 4), List(1, 2, 6, 2, 3), List(2, 1, 6, 2, 3), List(3, 5, 1, 3))

  def measure(run: => Option[Board]): (Option[Board], Long) = {
    val start = System.currentTimeMillis()
    val result = run
    val end = System.currentTimeMillis()
    (result, end - start)
  }

  def solveAndMeasure(rc: Clues, cc: Clues): Unit = {
    val (board, runtime) = measure(NonogramSolver.solve(rc, cc))
    println(board)
    println
    println(s"Board is valid? ${if (board.isEmpty) false else board.get.isValid(rc, cc)} after $runtime milliseconds.")
  }

  solveAndMeasure(rc20, cc20)
}
