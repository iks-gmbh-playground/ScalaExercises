package com.iks.codewars.assembler.complex

import scala.annotation.tailrec

object MessageHandling {
  object MessagePart extends Enumeration {
    val Text, Number, Register, Invalid = Value
  }

  import MessagePart._

  def parseMessage(msg: String): (MessagePart.Value, String, Option[String]) = {
    import MessagePart._

    val tail = "(,\\s(.*))?"
    val startsWithString = ("'(.+?)'" + tail).r
    val startsWithInt = ("(\\d+)" + tail).r
    val startsWithName = ("([A-Za-z]+\\S*)" + tail).r
    msg match {
      case startsWithString(text, _, tail) => (Text, text, Option(tail))
      case startsWithInt(number, _, tail) => (Number, number, Option(tail))
      case startsWithName(reg, _, tail) => (Register, reg, Option(tail))
      case _ => (Invalid, "", None)
    }
  }

  @tailrec
  def constructMessage(registers: Registers, text: String, acc: String = ""): Either[String, String] = {
    if (text.isEmpty) Right(acc)
    else parseMessage(text) match {
      case (Text, text, tail) => constructMessage(registers, tail.getOrElse(""), acc + text)
      case (Register, reg, tail) =>
        if (registers.contains(reg)) constructMessage(registers, tail.getOrElse(""), acc + registers(reg))
        else Left(s"$reg ist not a register")
      case (Number, number, tail) => constructMessage(registers, tail.getOrElse(""), acc + number.toInt)
      case _ => Left(s"cannot construct output from $text")
    }
  }

}

object InstructionHandling {
  def cleanLine(line: String): Option[String] = line.trim match {
    case s";$_" => None
    case "" => None
    case s"$instruction;$_" => Some(compressBlanks(instruction.trim))
    case instruction => Some(compressBlanks(instruction))
  }

  def compressBlanks(line: String): String = {
    if (line.contains('\'')) {
      val firstIndex = line.indexOf('\'')
      val lastIndex = line.lastIndexOf('\'')
      compressBlanks(line.substring(0, firstIndex)) + " " + line.slice(firstIndex, lastIndex + 1) + compressBlanks(line.substring(lastIndex + 1))
    } else line.split("\\s+").mkString(" ")
  }
}