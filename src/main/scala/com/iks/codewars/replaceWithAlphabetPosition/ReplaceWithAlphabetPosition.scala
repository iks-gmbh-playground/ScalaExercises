package com.iks.codewars.replaceWithAlphabetPosition

object ReplaceWithAlphabetPosition {

  def alphabetPosition(text: String): String = {
    text.map(_.toLower.toInt).collect { case i if (97 to 122).contains(i) => (i - 96).toString } .mkString(" ")
  }
}
