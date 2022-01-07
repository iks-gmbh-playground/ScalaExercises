package com.iks.codewars.texasHoldemHands

import org.scalatest.funsuite.AnyFunSuite

class PokerTest extends AnyFunSuite {
  test("Nothing") {
    val holeCards = List("K♠", "A♦")
    val communityCards = List("J♣", "Q♥", "9♥", "2♥", "3♦")
    val expected = ("nothing", List("A", "K", "Q", "J", "9"))
    test(holeCards, communityCards, expected)
  }

  test("Pair") {
    val holeCards = List("K♠", "Q♦")
    val communityCards = List("J♣", "Q♥", "9♥", "2♥", "3♦")
    val expected = ("pair", List("Q", "K", "J", "9"))
    test(holeCards, communityCards, expected)
  }

  test("Two pair") {
    val holeCards = List("K♠", "J♦")
    val communityCards = List("J♣", "K♥", "9♥", "2♥", "3♦")
    val expected = ("two pair", List("K", "J", "9"))
    test(holeCards, communityCards, expected)
  }

  test("Three of a kind") {
    val holeCards = List("4♠", "9♦")
    val communityCards = List("J♣", "Q♥", "Q♠", "2♥", "Q♦")
    val expected = ("three-of-a-kind", List("Q", "J", "9"))
    test(holeCards, communityCards, expected)
  }

  test("Straight") {
    val holeCards = List("Q♠", "2♦")
    val communityCards = List("J♣", "10♥", "9♥", "K♥", "3♦")
    val expected = ("straight", List("K", "Q", "J", "10", "9"))
    test(holeCards, communityCards, expected)
  }

  test("flush") {
    val holeCards = List("A♠", "K♦")
    val communityCards = List("J♥", "5♥", "10♥", "Q♥", "3♥")
    val expected = ("flush", List("Q", "J", "10", "5", "3"))
    test(holeCards, communityCards, expected)
  }

  test("Full house") {
    val holeCards = List("A♠", "A♦")
    val communityCards = List("K♣", "K♥", "A♥", "Q♥", "3♦")
    val expected = ("full house", List("A", "K"))
    test(holeCards, communityCards, expected)
  }

  test("Four of a kind") {
    val holeCards = List("2♠", "3♦")
    val communityCards = List("2♣", "2♥", "3♠", "3♥", "2♦")
    val expected = ("four-of-a-kind", List("2", "3"))
    test(holeCards, communityCards, expected)
  }

  test("Straight flush") {
    val holeCards = List("8♠", "6♠")
    val communityCards = List("7♠", "5♠", "9♠", "J♠", "10♠")
    val expected = ("straight-flush", List("J", "10", "9", "8", "7"))
    test(holeCards, communityCards, expected)
  }

  private def test(holeCards: List[String], communityCards: List[String], expected: (String, List[String])): Unit =
    withClue(s"""${holeCards.mkString(" ")} -- ${communityCards.mkString(" ")}\n""") {
      assert(Poker.hand(holeCards, communityCards) === expected)
    }
}
