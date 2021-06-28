package com.iks.codewars.assembler.complex

import scala.annotation.tailrec
import scala.collection.Map

//This is the second part of this kata series. First part is here.
//
//We want to create an interpreter of assembler which will support the following instructions:
//
//  mov x, y - copy y (either an integer or the value of a register) into register x.
//  inc x - increase the content of register x by one.
//  dec x - decrease the content of register x by one.
//  add x, y - add the content of the register x with y (either an integer or the value of a register) and stores the result in x (i.e. register[x] += y).
//  sub x, y - subtract y (either an integer or the value of a register) from the register x and stores the result in x (i.e. register[x] -= y).
//  mul x, y - same with multiply (i.e. register[x] *= y).
//  div x, y - same with integer division (i.e. register[x] /= y).
//  label: - define a label position (label = identifier + ":", an identifier being a string that does not match any other command).
//           Jump commands and call are aimed to these labels positions in the program.
//  jmp lbl - jumps to the label lbl.
//  cmp x, y - compares x (either an integer or the value of a register) and y (either an integer or the value of a register).
//             The result is used in the conditional jumps (jne, je, jge, jg, jle and jl)
//  jne lbl - jump to the label lbl if the values of the previous cmp command were not equal.
//  je lbl - jump to the label lbl if the values of the previous cmp command were equal.
//  jge lbl - jump to the label lbl if x was greater or equal than y in the previous cmp command.
//  jg lbl - jump to the label lbl if x was greater than y in the previous cmp command.
//  jle lbl - jump to the label lbl if x was less or equal than y in the previous cmp command.
//  jl lbl - jump to the label lbl if x was less than y in the previous cmp command.
//  call lbl - call to the subroutine identified by lbl. When a ret is found in a subroutine, the instruction pointer
//             should return to the instruction next to this call command.
//  ret - when a ret is found in a subroutine, the instruction pointer should return to the instruction that called the current function.
//  msg 'Register: ', x - this instruction stores the output of the program. It may contain text strings
//                        (delimited by single quotes) and registers. The number of arguments isn't limited and will vary, depending on the program.
//  end - this instruction indicates that the program ends correctly, so the stored output is returned
//        (if the program terminates without this instruction it should return the default output: see below).
//  ; comment - comments should not be taken in consideration during the execution of the program.
//
//  Output format:
//  The normal output format is a string (returned with the end command). For Scala and Rust programming languages it should be encapsulated into Option.
//
//  If the program does finish itself without using an end instruction, the default return value is:
//
//  None
//
//Input format:
//  The function/method will take as input a multiline string of instructions, delimited with EOL characters.
//  Please, note that the instructions may also have indentation for readability purposes.
//
//  For example:
//
//  val program = "\n; My first program\nmov  a, 5\ninc  a\ncall function\nmsg  '(5+1)/2 = ', a    ; output message\nend\n\nfunction:\n    div  a, 2\n    ret\n"
//  AssemblerInterpreter.interpret(program);
//
//  Which is equivalent to (keep in mind that empty lines are not displayed in the console on CW, so you actually won't see the separation before "function:"...):
//
//  ; My first program
//  mov  a, 5
//  inc  a
//  call function
//  msg  '(5+1)/2 = ', a    ; output message
//  end
//
//  function:
//    div  a, 2
//    ret
//
//  The above code would set register a to 5, increase its value by 1, calls the subroutine function, divide its value by 2,
//  returns to the first call instruction, prepares the output of the program and then returns it with the end instruction.
//  In this case, the output would be (5+1)/2 = 3.

object AssemblerInterpreter extends App {

  type Registers = Map[String, Int]
  type Error = String
  type Output = String
  type Instructions = IndexedSeq[String]

  def interpret(input: String): Option[String] = {
    def untilErrorOrEnd(program: Program): Either[Error, Output] =
      program.executeCurrentInstruction.
        flatMap(pgm => pgm.endReached.
          flatMap(atEnd => if (atEnd) Right(pgm.output) else untilErrorOrEnd(pgm)))

    untilErrorOrEnd(Program(input.split("\n").toIndexedSeq)) match {
      case Left(error) =>
        println(error)
        None
      case Right(output) =>
        Some(output)
    }
  }

  val input = "\n; My first program\nmov  a, 5\ninc  a\ncall function\nmsg  '(5+1)/2 = ', a    ; output message\nend\n\nfunction:\n    div  a, 2\n    ret\n"

  println(interpret(input))
}
