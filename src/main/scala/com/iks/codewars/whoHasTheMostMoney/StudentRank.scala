package com.iks.codewars.whoHasTheMostMoney

// You're going on a trip with some students and it's up to you to keep track of how much money each Student has.
// A student is defined like this:
//
//    case class Student(name: String, fives: Int, tens: Int, twenties: Int)
//
// As you can tell, each Student has some fives, tens, and twenties. Your job is to return the name of the student
// with the most money. If every student has the same amount, then return "all".
//
// Notes:
//    Each student will have a unique name
//    There will always be a clear winner: either one person has the most, or everyone has the same amount
//    If there is only one student, then that student has the most money

object StudentRank {
  case class Student(name: String, fives: Int, tens: Int, twenties: Int)

  def hasMoney(student: Student): Int =
    student.fives * 5 + student.tens * 10 + student.twenties * 20

  def mostMoney(students: List[Student]): String = students match {
    case only :: Nil => only.name
    case _ =>
      val winner = students.maxBy(hasMoney)
      if (students.exists(hasMoney(_) != hasMoney(winner))) winner.name
      else "all"
  }

  def mostMoneyBetter(students: List[Student]): String =
    if (students.groupBy(hasMoney).size == 1 && students.size != 1) "all"
    else students.maxBy(hasMoney).name


}
