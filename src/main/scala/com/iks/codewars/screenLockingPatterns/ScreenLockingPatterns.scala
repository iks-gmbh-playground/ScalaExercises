package com.iks.codewars.screenLockingPatterns

// Screen Locking Patterns (https://www.codewars.com/kata/585894545a8a07255e0002f1/train/scala)
// You might already be familiar with many smartphones that allow you to use a geometric pattern as a security measure.
// To unlock the device, you need to connect a sequence of dots/points in a grid by swiping your finger without
// lifting it as you trace the pattern through the screen.
//
// The image below has an example pattern of 7 dots/points: (A -> B -> I -> E -> D -> G -> C).
//
//  A  B  C
//  D  E  F
//  G  H  I
//
//  For this kata, your job is to implement a function that returns the number of possible patterns starting
//  from a given first point, that have a given length.
//
//  More specifically, for a function countPatternsFrom(firstPoint, length), the parameter
//  firstPoint is a single-character string corresponding to the point in the grid (i.e.: 'A') where your patterns start,
//  and the parameter length is an integer indicating the number of points (length) every pattern must have.
//
//  For example, countPatternsFrom("C", 2), should return the number of patterns starting from 'C' that have 2 two points. The return value in this case would be 5, because there are 5 possible patterns:
//
//  (C -> B), (C -> D), (C -> E), (C -> F) and (C -> H).
//
//  Bear in mind that this kata requires returning the number of patterns, not the patterns themselves,
//  so you only need to count them. Also, the name of the function might be different depending on the programming
//  language used, but the idea remains the same.
//
//  Rules
//  1. In a pattern, the dots/points cannot be repeated: they can only be used once, at most.
//  2. In a pattern, any two subsequent dots/points can only be connected with direct straight lines in either of these ways:
//  3. Horizontally: like (A -> B) in the example pattern image.
//  4. Vertically: like (D -> G) in the example pattern image.
//  5. Diagonally: like (I -> E), as well as (B -> I), in the example pattern image.
//  6. Passing over a point between them that has already been 'used': like (G -> C) passing over E,
//     in the example pattern image. This is the trickiest rule. Normally, you wouldn't be able to connect
//     G to C, because E is between them, however when E has already been used as part the pattern you are
//     tracing, you can connect G to C passing over E, because E is ignored, as it was already used once.
//
//  The sample tests have some examples of the number of combinations for some cases to help you check your code.
//
//  Haskell Note: A data type Vertex is provided in place of the single-character strings.
//  See the solution setup code for more details.
//
//  Fun fact:
//
//  In case you're wondering out of curiosity, for the Android lock screen, the valid patterns must have
//  between 4 and 9 dots/points. There are 389112 possible valid patterns in total;
//  that is, patterns with a length between 4 and 9 dots/points.

object ScreenLockingPatterns {
  def neighboursOf(c: Char): Set[Char] = c match {
    case 'A' => "DHEFB".toSet
    case 'B' => "ADGEIFC".toSet
    case 'C' => "BDEHF".toSet
    case 'D' => "GHIECBA".toSet
    case 'E' => "ADGHIFCB".toSet
    case 'F' => "CBAEGHI".toSet
    case 'G' => "HFEBD".toSet
    case 'H' => "IFCEADG".toSet
    case 'I' => "FBEDH".toSet
    case _ => Set()
  }

  def hiddenNeighboursOf(c: Char): Map[Char, Char] = c match {
    case 'A' => Map('D' -> 'G', 'E' -> 'I', 'B' -> 'C')
    case 'B' => Map('E' -> 'H')
    case 'C' => Map('B' -> 'A', 'E' -> 'G', 'F' -> 'I')
    case 'D' => Map('E' -> 'F')
    case 'F' => Map('E' -> 'D')
    case 'G' => Map('D' -> 'A', 'E' -> 'C', 'H' -> 'I')
    case 'H' => Map('E' -> 'B')
    case 'I' => Map('H' -> 'G', 'E' -> 'A', 'F' -> 'C')
    case _ => Map()
  }

  def actualNeighboursOf(d: Char, visited: Set[Char]): Set[Char] =
    neighboursOf(d).diff(visited).
      union(neighboursOf(d).intersect(visited).
        foldLeft(Set[Char]())((acc, n) =>
          hiddenNeighboursOf(d).get(n).
            map(h => if (visited.contains(h)) acc else acc + h).
            getOrElse(acc)))

  def countPatternsFrom(f: Char, l: Int,
                        visited: Set[Char] = Set()): Int =
    l match {
      case 1 => 1
      case _ if l < 1 => 0
      case _ =>
        actualNeighboursOf(f, visited).
          foldLeft(0)((acc, n) => acc + countPatternsFrom(n, l - 1, visited + f))
    }
}
