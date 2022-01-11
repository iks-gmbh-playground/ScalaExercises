package com.iks.codewars.nonograms

import com.iks.codewars.nonograms.NonogramSolver.Board
import org.scalatest.Assertions.assertResult
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NonogramSolverSpec extends AnyFlatSpec with Matchers {

  it should "pass basic test1" in {
    NonogramSolverSpec.test1()
    NonogramSolverSpec.test2()
    NonogramSolverSpec.test3()
  }

  it should "pass basic test4" in {
    NonogramSolverSpec.test4()
  }

}

object NonogramSolverSpec {
  //  clues = (((1, 1), (4,), (1, 1, 1), (3,), (1,)),
  //    ((1,), (2,), (3,), (2, 1), (4,)))
  //
  //  ans = ((0, 0, 1, 0, 0),
  //    (1, 1, 0, 0, 0),
  //    (0, 1, 1, 1, 0),
  //    (1, 1, 0, 1, 0),
  //    (0, 1, 1, 1, 1))
  //
  private def test1(): Unit = {
    val rowClues = List(List(1), List(2), List(3), List(2, 1), List(4))
    val columnClues = List(List(1, 1), List(4), List(1, 1, 1), List(3), List(1))

    val expected = Board(List("00100", "11000", "01110", "11010", "01111"))
    assertResult(Some(expected)){ NonogramSolver.solve(rowClues, columnClues) }
  }

  //  clues = (((1,), (3,), (1,), (3, 1), (3, 1)),
  //    ((3,), (2,), (2, 2), (1,), (1, 2)))
  //
  //  ans = ((0, 0, 1, 1, 1),
  //    (0, 0, 0, 1, 1),
  //    (1, 1, 0, 1, 1),
  //    (0, 1, 0, 0, 0),
  //    (0, 1, 0, 1, 1))
  //
  private def test2(): Unit = {
    val rowClues = List(List(3), List(2), List(2, 2), List(1), List(1, 2))
    val columnClues = List(List(1), List(3), List(1), List(3, 1), List(3, 1))

    val expected = Board(List("00111", "00011", "11011", "01000", "01011"))
    assertResult(Some(expected)){ NonogramSolver.solve(rowClues, columnClues) }
  }

  //  clues = (((3,), (2,), (1, 1), (2,), (4,)),
  //    ((2,), (3, 1), (1, 2), (3,), (1,)))
  //
  //  ans = ((1, 1, 0, 0, 0),
  //    (1, 1, 1, 0, 1),
  //    (1, 0, 0, 1, 1),
  //    (0, 0, 1, 1, 1),
  //    (0, 0, 0, 0, 1))
  //
  private def test3(): Unit = {
    val rowClues = List(List(2), List(3, 1), List(1, 2), List(3), List(1))
    val columnClues = List(List(3), List(2), List(1, 1), List(2), List(4))

    val expected = Board(List("11000", "11101", "10011", "00111", "00001"))
    assertResult(Some(expected)){ NonogramSolver.solve(rowClues, columnClues) }
  }

  private def test4(): Unit = {
    val rowClues = List(List(2), List(2), List(1), List(2, 2), List(1, 3))
    val columnClues = List(List(2), List(2), List(1), List(2, 2), List(2, 2))

    val expected = Board(List("00011", "00011", "01000", "11011", "10111"))
    assertResult(Some(expected)){ NonogramSolver.solve(rowClues, columnClues) }
  }

  private def hard25(): Unit = {
    val rowClues = List(List(3, 3, 4), List(1, 1, 4), List(1, 1, 3, 1), List(2, 1, 3), List(3, 1, 1, 4), List(1, 1), List(1, 3, 3, 3, 2), List(6, 4, 3), List(6, 8, 3), List(19), List(2, 10, 3), List(2, 9, 4), List(1, 5, 3), List(1, 1, 1, 3), List(7, 3), List(1, 1, 10), List(1, 3, 6), List(3), List(1, 4, 2, 2), List(1, 5, 3, 2))
    val columnClues = List(List(5, 6, 2, 2), List(1, 2, 5), List(3, 1, 3), List(4), List(3, 4), List(3, 8, 1, 1), List(5, 3, 2), List(1, 1, 4, 2), List(1, 1, 4, 2), List(1, 7, 2), List(5, 3), List(6, 3, 1), List(7, 3, 2), List(10, 2), List(2, 5, 3), List(2, 1, 3), List(1, 3, 3, 2), List(1, 1, 1, 11), List(1, 15), List(1, 3, 9))

    val expected = Board(List("00011", "00011", "01000", "11011", "10111"))
    assertResult(Some(expected)){ NonogramSolver.solve(rowClues, columnClues) }
  }

}
