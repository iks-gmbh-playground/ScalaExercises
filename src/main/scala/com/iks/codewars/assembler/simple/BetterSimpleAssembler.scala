package com.iks.codewars.assembler.simple

import scala.collection.mutable

object BetterSimpleAssembler {
  val registers: mutable.Map[String, Int] = mutable.Map()
  var pointer: Int = 0
  def interpret(instructions: List[String]): Map[String, Int] = {
    while(pointer < instructions.length) {
      instructions(pointer) match {
        case s"mov $x $y" =>
          registers += (x -> y.toIntOption.getOrElse(registers(y)))
          pointer += 1
        case s"inc $r" =>
          registers(r) += 1
          pointer += 1
        case s"inc $r $v" =>
          registers(r) += 1
          pointer += 1
        case s"dec $r" =>
          registers(r) -= 1
          pointer += 1
        case s"dec $r $v" =>
          registers(r) -= 1
          pointer += 1
        case s"jnz $x $y" =>
          if (x.toIntOption.getOrElse(registers(x)) == 0) pointer += 1
          else pointer += y.toIntOption.getOrElse(registers(y))
        case _ => throw new IllegalArgumentException()
      }
    }
    registers.toMap
  }

}
