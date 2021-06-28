package com.iks.codewars.assembler.simple

import scala.annotation.tailrec

// valid commands to interpret:
// mov x y - copies y (either a constant value or the content of a register) into register x
// inc x - increases the content of the register x by one
// dec x - decreases the content of the register x by one
// jnz x y - jumps to an instruction y steps away (positive means forward, negative means backward, y can be a register or a constant), but only if x (a constant or a register) is not zero

object SimpleAssembler {

  type Registers = Map[String, Int]
  type Program = IndexedSeq[Instruction]
  type Pointer = Int
  type ProgramState = (Program, Pointer, Registers)
  type Result = Either[String, Registers]
  type InstructionResult = Either[String, ProgramState]

  trait Instruction {
    def execute(state: ProgramState): InstructionResult
  }
  case class MoveRegister(toRegister: String, fromRegister: String) extends Instruction {
    override def execute(state: ProgramState): InstructionResult = {
      val (pgm, ptr, regs) = state
      if (regs.isDefinedAt(fromRegister)) Right((pgm, ptr + 1, regs + (toRegister -> regs(fromRegister))))
      else Left(s"MoveRegister: Register $fromRegister does not exist!")
    }
  }
  case class MoveValue(toRegister: String, value: Int) extends Instruction {
    override def execute(state: ProgramState): InstructionResult = {
      val (pgm, ptr, regs) = state
      Right((pgm, ptr + 1, regs + (toRegister -> value)))
    }
  }
  case class Increase(register: String) extends Instruction {
    override def execute(state: ProgramState): InstructionResult = {
      val (pgm, ptr, regs) = state
      if (regs.isDefinedAt(register)) Right((pgm, ptr + 1, regs + (register -> (regs(register) + 1))))
      else Left(s"Increase: Register $register does not exist!")
    }
  }
  case class Decrease(register: String) extends Instruction {
    override def execute(state: ProgramState): InstructionResult = {
      val (pgm, ptr, regs) = state
      if (regs.isDefinedAt(register)) Right((pgm, ptr + 1, regs + (register -> (regs(register) - 1))))
      else Left(s"Decrease: Register $register does not exist!")
    }
  }
  trait Jump {
    def jumpIfNotZero(test: Int, ptr: Pointer, distance: Int): Pointer =
      if (test != 0) ptr + distance else ptr + 1
  }
  case class JumpRegVal(testReg: String, distance: Int) extends Instruction with Jump {
    override def execute(state: ProgramState): InstructionResult = {
      val (pgm, ptr, regs) = state
      if (regs.isDefinedAt(testReg)) Right((pgm, jumpIfNotZero(regs(testReg), ptr, distance), regs))
      else Left(s"JumpRegVal: Register $testReg does not exist!")
    }
  }
  case class JumpRegReg(testReg: String, distanceReg: String) extends Instruction with Jump {
    override def execute(state: ProgramState): InstructionResult = {
      val (pgm, ptr, regs) = state
      if (regs.isDefinedAt(testReg))
        if (regs.isDefinedAt(distanceReg)) Right((pgm, jumpIfNotZero(regs(testReg), ptr, regs(distanceReg)), regs))
        else Left(s"JumpRegReg: Register $distanceReg does not exist!")
      else Left(s"JumpRegReg: Register $testReg does not exist!")
    }
  }
  case class JumpValVal(test: Int, distance: Int) extends Instruction with Jump {
    override def execute(state: ProgramState): InstructionResult = {
      val (pgm, ptr, regs) = state
      Right((pgm, jumpIfNotZero(test, ptr, distance), regs))
    }
  }
  case class JumpValReg(test: Int, distanceReg: String) extends Instruction with Jump {
    override def execute(state: ProgramState): InstructionResult = {
      val (pgm, ptr, regs) = state
      if (regs.isDefinedAt(distanceReg))
        Right((pgm, jumpIfNotZero(test, ptr, regs(distanceReg)), regs))
      else Left(s"JumpRegReg: Register $distanceReg does not exist!")
    }
  }

  @tailrec
  private def execute(state: ProgramState): Either[String,Registers] = {
    val (pgm, ptr, regs) = state
    if (ptr >= pgm.length) Right(regs)
    else pgm(ptr).execute(state) match {
      case Right(newState) => execute(newState)
      case Left(errorMsg) => Left(errorMsg)
    }
  }

  def interpret(instructionList: List[String]): Map[String, Int] = {
    val program = toProgram(instructionList)
    execute(program, 0, Map[String, Int]()) match {
      case Right(registers) => registers
      case Left(errMsg) =>
        println(errMsg)
        Map[String, Int]()
    }
  }

  private def toProgram(program: List[String]): IndexedSeq[Instruction] = {
    (for (command <- program) yield {
      command.split(" ") match {
        case Array("mov", regX, y) if y.matches("-?\\d+") => MoveValue(regX, y.toInt)
        case Array("mov", regX, regY) => MoveRegister(regX, regY)
        case Array("inc", regX) => Increase(regX)
        case Array("dec", regX) => Decrease(regX)
        case Array("jnz", x, y) if x.matches("-?\\d+") && y.matches("-?\\d+") => JumpValVal(x.toInt, y.toInt)
        case Array("jnz", regX, y) if y.matches("-?\\d+") => JumpRegVal(regX, y.toInt)
        case Array("jnz", x, regY) if x.matches("-?\\d+") => JumpValReg(x.toInt, regY)
        case Array("jnz", regX, regY) => JumpRegReg(regX, regY)
        case array =>
          println(s"toProgram: MatchError ${array.mkString("Array(", ", ", ")")}")
          throw new IllegalArgumentException(s"toProgram: MatchError ${array.mkString("Array(", ", ", ")")}")
      }
    }).toIndexedSeq
  }
}
