package com.iks.algorithms.maximumPairwiseProduct

import java.util.Scanner
import scala.annotation.tailrec
import scala.util.Random

object MaximumPairwiseProduct extends App {

  def findMaximumPairwiseProductNonNegative(ints: Seq[Int]): Long = {
    val (max1, max2) = ints.foldLeft((Int.MinValue, Int.MinValue)) {
      case ((first, second), i) =>
        if (i > first) (i, first)
        else if (i > second) (first, i)
        else (first, second)
    }
    max1.toLong * max2
  }

  def findMaximumPairwiseProduct(ints: Seq[Int]): Long =
    if (ints.size < 2) 0
    else (for {
      i <- ints.indices
      j <- ints.indices.drop(i + 1)
    } yield ints(i).toLong * ints(j)).max

  @tailrec
  def stressTest(maxN: Int, maxM: Int): Unit = {
    val n = Random.nextInt(maxN - 2) + 2
    val ints = for (i <- 1 to n) yield Random.nextInt(maxM)
    print(s"$ints: ")
    val expected = findMaximumPairwiseProduct(ints)
    val actual = findMaximumPairwiseProductNonNegative(ints)
    if (expected == actual) {
      println("OK")
      stressTest(maxN, maxM)
    } else println(s"wrong result: expected $expected but gut $actual")
  }

  stressTest(5, 9)
}
