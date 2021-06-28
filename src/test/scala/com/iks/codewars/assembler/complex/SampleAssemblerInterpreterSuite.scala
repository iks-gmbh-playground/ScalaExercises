package com.iks.codewars.assembler.complex

import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec

class SampleAssemblerInterpreterSuite extends AnyFunSpec with OptionValues {

  import AssemblerInterpreter._

  describe("first iteration") {
    val program = "msg 'first iteration'\nend"
    val expected = Some("first iteration")
    check_program(program, expected)
  }

  describe("handle mov instruction") {
    val initialProgram = Program(instructions = IndexedSeq("mov a, 1", "mov a, b"), registers = Map("b" -> 3))
    val expectedRegisters = List(Map("a" -> 1, "b" -> 3), Map("a" -> 3, "b" -> 3))
    for ( (instruction, expected) <- initialProgram.instructions.zip(expectedRegisters) ) {
      val actualResult = initialProgram.execute(instruction) match {
        case Right(pgm) => pgm.registers
        case Left(error) =>
          println(error)
          initialProgram.registers
      }
      it(s"mov execution should return '$expected' and actually returned '$actualResult'") {
        assert(actualResult === expected)
      }
    }
  }

  describe("handle label") {
    val initialProgram = Program(instructions = IndexedSeq("label:", "label: inc a"), registers = Map("a" -> 3))

    val expectedRegisters = List(Map("a" -> 3), Map("a" -> 4))
    for ( (instruction, expected) <- initialProgram.instructions.zip(expectedRegisters) ) {
      val (registers, instructionPointer) = initialProgram.execute(instruction) match {
        case Right(pgm) => (pgm.registers, pgm.instructionPointer)
        case Left(error) =>
          println(error)
          (initialProgram.registers, 0)
      }
      it(s"label execution should return '($expected, 1)' and actually returned '($registers, $instructionPointer)'") {
        assert(registers === expected)
        assert(instructionPointer === 1)
      }
    }
  }

  describe("handle inc instruction") {
    val initialProgram = Program(instructions = IndexedSeq("inc regA", "inc regB", "inc noReg"), registers = Map("regA" -> 42, "regB" -> 67))

    val expectedRegisters = List(Map("regA" -> 43, "regB" -> 67), Map("regA" -> 42, "regB" -> 68), Map("regA" -> 42, "regB" -> 67))
    for ( (instruction, expected) <- initialProgram.instructions.zip(expectedRegisters) ) {
      val actualResult = initialProgram.execute(instruction) match {
        case Right(pgm) => pgm.registers
        case Left(error) =>
          println(error)
          initialProgram.registers
      }
      it(s"inc execution should return '$expected' and actually returned '$actualResult'") {
        assert(actualResult === expected)
      }
    }
  }

  describe("handle dec instruction") {
    val instructions = IndexedSeq(
      "mov a , 5",
      "inc a",
      "call double",
      "dec a",
      "msg 'the result of (5 + 1) * 2 - 1 = ', a",
      "end",
      "double:",
      "mul a, 2",
      "ret"
    )
    val registers = Map("a" -> 13)
    val initialProgram = Program(instructions = instructions, instructionPointer = 3, registers = registers)
    val expected = Map("a" -> 12)
    initialProgram.executeCurrentInstruction match {
      case Right(pgm) =>
        it(s"dec should return $expected but actually returned ${pgm.registers}") {
          assert(pgm.registers === expected)
        }
      case Left(error) =>
        println(error)
        fail(error)
    }
  }

  describe("handle mul instruction") {
    val instructions = IndexedSeq(
      "mov a , 5",
      "inc a",
      "call double",
      "dec a",
      "msg 'the result of (5 + 1) * 2 - 1 = ', a",
      "end",
      "double:",
      "mul a, 2",
      "ret"
    )
    val registers = Map("a" -> 13)
    val initialProgram = Program(instructions = instructions, instructionPointer = 7, registers = registers)
    val expected = Map("a" -> 26)
    initialProgram.executeCurrentInstruction match {
      case Right(pgm) =>
        it(s"mul should return $expected but actually returned ${pgm.registers}") {
          assert(pgm.registers === expected)
        }
      case Left(error) =>
        println(error)
        it("mul should not return an error in this case") {
          fail(error)
        }
    }
  }

  describe("handle call instruction") {
    val initialProgram = Program(instructions = IndexedSeq("call function", "inc a", "msg '5+1  =', a", "end", "function: mov a, 5", "ret"),
      registers = Map("a" -> 42, "regB" -> 67))

    val instruction = initialProgram.instructions(0)

    initialProgram.execute(instruction) match {
      case Right(pgm) =>
        it(s"call should move the instruction pointer to 4 but actually moved it to ${pgm.instructionPointer}") {
          assert(pgm.instructionPointer === 4)
        }
      case Left(error) =>
        println(error)
        it(s"call in this case should be handled correctly") {
          fail(s"call: excuteInstrcution returned an error: $error ")
        }
    }
  }

  describe("handle erroneous call instruction") {
    val initialProgram = Program(instructions = IndexedSeq("call function", "inc a", "msg '5+1  =', a", "end", "label: mov a, 5", "ret"),
      registers = Map("a" -> 42, "regB" -> 67)
    )

    val instruction = initialProgram.instructions(0)

    initialProgram.execute(instruction) match {
      case Right(_) =>
        it(s"call should result in an error.") {
          fail("call: excute should return an error")
        }
      case Left(error) =>
        println(error)
        it("call in this case should return an error.") {
          assert(error === "Label function not found!")
        }
    }
  }

  describe("handle ret instruction") {
    val initialProgram = Program(instructions = IndexedSeq("call function", "inc a", "msg '5+1  =', a", "end", "label: mov a, 5", "ret"),
      instructionPointer = 5,
      callStack = List(0), registers = Map("a" -> 42, "regB" -> 67)
    )

    initialProgram.executeCurrentInstruction match {
      case Right(pgm) =>
        it(s"ret should move the instruction pointer to the call instruction + 1 (1)") {
          assert(pgm.instructionPointer === 1)
        }
      case Left(error) =>
        println(error)
        it("ret in this case should not return an error.") {
          fail(error)
        }
    }
  }

  describe("handle very simple programs") {
    val simplePrograms = Array(
      "\n; just one move and no output\nmov  a, 5\nend\n\n",
      "\n; just one move and no output and no end\nmov  a, 5; the only instruction\n\n\n")

    val expectedResults = Array(
      Some(""),
      None
    )

    for ( (pgm, expected) <- simplePrograms.zip(expectedResults) ) {
      val actual = interpret(pgm)
      it(s"should return $expected and actually returned $actual") {
        assert(actual === expected)
      }
    }
  }

  describe("run full program") {
    val programString = "\n; My first program\nmov  a, 5\ninc  a\ncall function\nmsg  '(5+1)/2 = ', a    ; output message\nend\n\nfunction:\n    div  a, 2\n    ret\n"
    val expected = Some("(5+1)/2 = 3")
    val actual = interpret(programString)

    it(s"interpret should return $expected but actually returned $actual") {
      assert(actual === expected)
    }
  }

  describe("Simple tests") {
    val simplePrograms = Array(
      "\n; My first program\nmov  a, 5\ninc  a\ncall function\nmsg  '(5+1)/2 = ', a    ; output message\nend\n\nfunction:\n    div  a, 2\n    ret\n",
      "\nmov   a, 5\nmov   b, a\nmov   c, a\ncall  proc_fact\ncall  print\nend\n\nproc_fact:\n    dec   b\n    mul   c, b\n    cmp   b, 1\n    jne   proc_fact\n    ret\n\nprint:\n    msg   a, '! = ', c ; output text\n    ret\n",
      "\nmov   a, 8            ; value\nmov   b, 0            ; next\nmov   c, 0            ; counter\nmov   d, 0            ; first\nmov   e, 1            ; second\ncall  proc_fib\ncall  print\nend\n\nproc_fib:\n    cmp   c, 2\n    jl    func_0\n    mov   b, d\n    add   b, e\n    mov   d, e\n    mov   e, b\n    inc   c\n    cmp   c, a\n    jle   proc_fib\n    ret\n\nfunc_0:\n    mov   b, c\n    inc   c\n    jmp   proc_fib\n\nprint:\n    msg   'Term ', a, ' of Fibonacci series is: ', b        ; output text\n    ret\n",
      "\nmov   a, 11           ; value1\nmov   b, 3            ; value2\ncall  mod_func\nmsg   'mod(', a, ', ', b, ') = ', d        ; output\nend\n\n; Mod function\nmod_func:\n    mov   c, a        ; temp1\n    div   c, b\n    mul   c, b\n    mov   d, a        ; temp2\n    sub   d, c\n    ret\n",
      "\nmov   a, 81         ; value1\nmov   b, 153        ; value2\ncall  init\ncall  proc_gcd\ncall  print\nend\n\nproc_gcd:\n    cmp   c, d\n    jne   loop\n    ret\n\nloop:\n    cmp   c, d\n    jg    a_bigger\n    jmp   b_bigger\n\na_bigger:\n    sub   c, d\n    jmp   proc_gcd\n\nb_bigger:\n    sub   d, c\n    jmp   proc_gcd\n\ninit:\n    cmp   a, 0\n    jl    a_abs\n    cmp   b, 0\n    jl    b_abs\n    mov   c, a            ; temp1\n    mov   d, b            ; temp2\n    ret\n\na_abs:\n    mul   a, -1\n    jmp   init\n\nb_abs:\n    mul   b, -1\n    jmp   init\n\nprint:\n    msg   'gcd(', a, ', ', b, ') = ', c\n    ret\n",
      "\ncall  func1\ncall  print\nend\n\nfunc1:\n    call  func2\n    ret\n\nfunc2:\n    ret\n\nprint:\n    msg 'This program should return null'\n",
      "\nmov   a, 2            ; value1\nmov   b, 10           ; value2\nmov   c, a            ; temp1\nmov   d, b            ; temp2\ncall  proc_func\ncall  print\nend\n\nproc_func:\n    cmp   d, 1\n    je    continue\n    mul   c, a\n    dec   d\n    call  proc_func\n\ncontinue:\n    ret\n\nprint:\n    msg a, '^', b, ' = ', c\n    ret\n")

    val expected = Array(
      Some("(5+1)/2 = 3"),
      Some("5! = 120"),
      Some("Term 8 of Fibonacci series is: 21"),
      Some("mod(11, 3) = 2"),
      Some("gcd(81, 153) = 9"),
      None,
      Some("2^10 = 1024"))

    for ((prg, result) <- simplePrograms zip expected)
      check_program(prg, result)
  }

  def check_program(program: String, expected: Option[String]): Unit = {
    val actual = interpret(program)
    it(s"should return '$expected' and actual return '$actual'") {
      assert(actual === expected)
    }
  }
}