package com.iks.codewars.assembler.complex

import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec

class ProgramSuite extends AnyFunSpec with OptionValues {

  import InstructionHandling._
  import MessageHandling._

  describe("clean some lines") {
    val lines = List("   ;    comment    ", "   ", "   instructionA ;    comment", "   instructionB  ")
    val expected = List(None, None, Some("instructionA"), Some("instructionB"))

    for ((line, expect) <- lines.zip(expected)) {
      val actual = cleanLine(line)
      it(s"cleanLine should return '$expect' and actually returns '$actual' for $line") {
        assert(actual === expect)
      }
    }
  }

  describe("construct simple output") {
    val content = "'my age is: ', a"
    val registers = Map("a" -> 58)
    val expected = "my age is: 58"
    val actual = constructMessage(registers, content) match {
      case Right(value) => value
      case Left(value) => value
    }
    it(s"should return '$expected' and actually returned '$actual'") {
      assert(actual === expected)
    }
  }

  describe("construct complex output") {
    val content = s"'My age is ', a, ' and Irene is ', b, '. ', 'Our children are much younger, of course, ', c, ', ', d, ' and ', 'the youngest ', e, '. ', 'All together we are ', ${58+56+32+30+22}, ' years old.'"
    val registers = Map("a" -> 58, "b" -> 56, "c" -> 32, "d" -> 30, "e" -> 22)
    val expected = s"My age is 58 and Irene is 56. Our children are much younger, of course, 32, 30 and the youngest 22. All together we are ${58+56+32+30+22} years old."
    val actual =  constructMessage(registers, content) match {
      case Right(value) => value
      case Left(value) => value
    }
    it(s"should return '$expected' and actually returned '$actual'") {
      assert(actual === expected)
    }
  }

  describe("construct output should report a missing register") {
    val content = s"'My age is ', a, ' and Irene is ', b, '. ', 'Our children are much younger, of course, ', c, ', ', d, ' and ', 'the youngest ', e, '. ', 'All together we are ', ${58+56+32+30+22}, ' years old.'"
    val registers = Map("a" -> 58, "b" -> 56, "c" -> 32, "d" -> 30, "f" -> 22)
    val expected = s"e ist not a register"
    val actual =  constructMessage(registers, content) match {
      case Right(value) => value
      case Left(value) => value
    }
    it(s"should return '$expected' and actually returned '$actual'") {
      assert(actual === expected)
    }
  }
}
