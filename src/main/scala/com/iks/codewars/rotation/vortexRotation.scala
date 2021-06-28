package com.iks.codewars.rotation

import scala.annotation.tailrec

// https://www.codewars.com/kata/58d3cf477a4ea9bb2f000103/train/scala
//
// In most vortices, the fluid flow velocity is greatest next to its axis and decreases in inverse proportion to the distance from the axis.
// So the rotation speed increases with every ring nearer to the middle.
//
// For this kata this means, that the outer "ring" of the matrix rotates one step. The next ring rotates two steps. The next ring rotates three steps. And so on...
//
// The rotation is always "to the left", so against clockwise!
//
// Example: A square is rotated like this:
// 5 3 6 1  ->  1 4 3 2
// 5 8 7 4  ->  6 4 2 2
// 1 2 4 3  ->  3 7 8 1
// 3 1 2 2  ->  5 5 1 3

object vortexRotation {
  private def getInnersMatrix(matrix: Array[Array[Int]], length: Int): Array[Array[Int]] = {
    val inner = Array.fill(length-2)(Array.fill(length-2)(0))
    for (i <- 1 to length - 2; j <- 1 to length - 2)
      inner(i - 1)(j - 1) = matrix(i)(j)
    inner
  }

  private def replaceInner(matrix: Array[Array[Int]], inner: Array[Array[Int]], length: Int): Array[Array[Int]] = {
    for (i <- 1 to length - 2; j <- 1 to length - 2)
      matrix(i)(j) = inner(i - 1)(j - 1)
    matrix
  }

  @tailrec
  private def rotateOuterRing(matrix: Array[Array[Int]], length: Int, times: Int): Array[Array[Int]] = times match {
    case 0 => matrix
    case n =>
      val max = length-1
      val outer = Array.fill(length)(Array.fill(length)(0))
      for (i <- matrix.indices) {
        outer(0)(i) = matrix(i)(max)
        outer(i)(0) = matrix(0)(max - i)
        outer(max)(i) = matrix(i)(0)
        outer(max - i)(max) = matrix(max)(i)
      }
      rotateOuterRing(outer, length, n - 1)
  }

  def rotateLikeAVortex(matrix: Array[Array[Int]], times: Int = 1): Array[Array[Int]] = (matrix.length, times) match {
    case (1, _) => matrix
    case (2, 0) => matrix
    case (2, n) => rotateLikeAVortex(Array(Array(matrix(0)(1), matrix(1)(1)), Array(matrix(0)(0), matrix(1)(0))), n - 1)
    case (l, n) => {
      val newMatrix = rotateOuterRing(matrix, l, n)
      val innerMatrix = rotateLikeAVortex(getInnersMatrix(matrix, l), n + 1)
      replaceInner(newMatrix, innerMatrix, l)
    }
  }

}
