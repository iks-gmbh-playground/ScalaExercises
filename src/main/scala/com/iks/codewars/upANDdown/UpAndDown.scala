package com.iks.codewars.upANDdown

import scala.collection.mutable

object UpAndDown {
  def switch[A](buf: mutable.Buffer[A], i: Int): buf.type = buf.patchInPlace(i, Seq(buf(i + 1), buf(i)), 2)

  def arrange(s: String): String = {
    val words = mutable.ArrayBuffer(s.split(" +"): _*)

    words.indices.map(i => {
      if (i % 2 == 0)
        (if (i < words.length - 1 && words(i).length > words(i + 1).length) switch(words, i)(i)
        else words(i)).toLowerCase
      else
        (if (i < words.length - 1 && words(i).length < words(i + 1).length) switch(words, i)(i)
        else words(i)).toUpperCase
    }).mkString(" ")
  }
}
