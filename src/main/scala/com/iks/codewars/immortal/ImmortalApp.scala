package com.iks.codewars.immortal

object ImmortalApp extends App {

  def rect(n: Long, m: Long, l: Long = 0): IndexedSeq[Long] = for {
    i <- 0L until n
    j <- 0L until m
  } yield {
    val x = (i ^ j) - l
    if (x < 0) 0 else x
  }

  def results: Seq[(Int, Int, Long)] = for {
    n <- 1 to 10
    m <- 1 to n
  } yield (n, m, rect(n, m).sum)

  val n = 32
  val m = 21

  println(rect(n,m).zipWithIndex map { case (x, i) => (if (i % m == 0) "\n" else "") + x} mkString " ")

}
