package com.iks.codewars.nonograms

object NonogramRunner extends App {
  val rowClues = List(List(1), List(2), List(3), List(2, 1), List(4))
  val columnClues = List(List(1, 1), List(4), List(1, 1, 1), List(3), List(1))

  val rc10 = List(List(4), List(1, 3), List(2, 1), List(5), List(6), List(6), List(3, 2), List(5, 3), List(1, 1, 2), List(5))
  val cc10 = List(List(7), List(5, 1), List(9), List(4, 1, 1), List(3, 1, 1), List(2), List(2), List(2, 1), List(3, 3), List(1, 3))

  val start = System.currentTimeMillis()
  val board = NonogramSolver.solve(rc10, cc10).get
  println(board)
  println
  println(s"Board is valid? ${board.isValid(rc10, cc10)} after ${ System.currentTimeMillis() - start } milliseconds.")
}
