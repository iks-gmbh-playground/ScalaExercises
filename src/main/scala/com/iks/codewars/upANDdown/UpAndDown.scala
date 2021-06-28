package com.iks.codewars.upANDdown

import scala.collection.mutable

object UpAndDown {
  def swapAdjacent[String](buf: mutable.Buffer[String], i: Int): mutable.Buffer[String] =
    buf.patchInPlace(i, Seq(buf(i + 1), buf(i)), 2)

  def isNotLastIndex[String](i: Int, it: mutable.Buffer[String]): Boolean =
    i < it.size - 1

  def wordIsLongerThanNext(words: mutable.Buffer[String], i: Int): Boolean =
    words(i).length > words(i + 1). length

  def wordIsShorterThanNext(words: mutable.Buffer[String], i: Int): Boolean =
    words(i).length < words(i + 1). length

  def arrange(s: String): String = {
    val words = mutable.ArrayBuffer(s.split(" +"): _*)

    words.indices.map(i => {
      if (i % 2 == 0)
        (if (isNotLastIndex(i, words) && wordIsLongerThanNext(words, i)) swapAdjacent(words, i)(i)
        else words(i)).toLowerCase
      else
        (if (isNotLastIndex(i, words) && wordIsShorterThanNext(words, i)) swapAdjacent(words, i)(i)
        else words(i)).toUpperCase
    }).mkString(" ")
  }
}
