package com.iks.codewars.assembler.complex

case class Program(instructions: Instructions,
                   instructionPointer: Int = 0,
                   callStack: List[Int] = List.empty,
                   cmpResult: Option[Int] = None,
                   registers: Registers = Map(),
                   output: Output = "") {
  import Program._

  private val END: String = "end"

  def currentInstruction: Either[String, String] = instructions.lift(instructionPointer) match {
    case Some(instruction) => Right(instruction)
    case None => Left("Abnormal end:Instruction pointer ran out of program!")
  }

  def endReached: Either[String, Boolean] = currentInstruction.map(_ == END)

  def executeCurrentInstruction: Either[String, Program] =
    currentInstruction.flatMap(execute)

  def indexOf(label: String): Either[String, Int] = {
    val index = instructions.indexWhere(_.startsWith(label + ":"))
    if (index < 0) Left(s"Label $label not found!") else Right(index)
  }

  def call(label: String): Either[String, Program] =
    indexOf(label).map(index => this.copy(instructionPointer = index, callStack = instructionPointer :: callStack))

  def ret(): Either[String, Program] =
    (callStack.headOption match {
      case Some(index) => Right(this.copy(instructionPointer = index, callStack = callStack.tail))
      case None =>
        println("ret: call stack empty - continue with next instruction after ret!")
        Right(this)
    }).map(next)

  def mov(value: String, toReg: String): Either[String, Program] =
    getInt(registers, value).map(i => this.copy(registers = update(registers, toReg, i))).map(next)

  def regOp(reg: String, value: String, op: (Int, Int) => Int): Either[String, Program] =
    for {x <- getIntFrom(registers, reg)
         y <- getInt(registers, value)}
    yield next(this.copy(registers = update(registers, reg, op(x, y))))

  def cmp(valueX: String, valueY: String): Either[String, Program] =
    for {
      x <- getInt(registers, valueX)
      y <- getInt(registers, valueY)
    } yield next(this.copy(cmpResult = Some(x.compare(y))))

  def jmp(label: String): Either[String, Program] =
    indexOf(label).map(index => this.copy(instructionPointer = index))

  def compareAndJump(p: Int => Boolean)(label: String): Either[String, Program] =
    cmpResult.
      map(c => if (p(c)) jmp(label) else Right(next(this))).
      getOrElse(Left("no previous cmp before conditional jump"))

  def execute(instruction: String): Either[String, Program] = {
    val labelPattern = "([A-Za-z]+\\S*): ?(.*)?".r
    val retPattern = "ret( .*)?".r

    instruction match {
      case END =>
        println("End of Program reached!")
        Right(this)
      case labelPattern(_, inst) =>
        if (inst.isEmpty) Right(next(this))
        else execute(inst)
      case s"call $label" =>
        call(label)
      case retPattern(_) =>
        ret()
      case s"mov $toReg, $y" =>
        mov(y, toReg)
      case s"div $reg, $y" =>
        regOp(reg, y, _ / _)
      case s"mul $reg, $y" =>
        regOp(reg, y, _ * _)
      case s"add $reg, $y" =>
        regOp(reg, y, _ + _)
      case s"sub $reg, $y" =>
        regOp(reg, y, _ - _)
      case s"inc $reg" =>
        regOp(reg, "1", _ + _)
      case s"dec $reg" =>
        regOp(reg, "1", _ - _)
      case s"cmp $x, $y" =>
        cmp(x, y)
      case s"jmp $label" =>
        jmp(label)
      case s"jne $label" =>
        compareAndJump(_ != 0)(label)
      case s"je $label" =>
        compareAndJump(_ == 0)(label)
      case s"jge $label" =>
        compareAndJump(_ >= 0)(label)
      case s"jg $label" =>
        compareAndJump(_ > 0)(label)
      case s"jle $label" =>
        compareAndJump(_ <= 0)(label)
      case s"jl $label" =>
        compareAndJump(_ < 0)(label)
      case s"msg $text" =>
        import MessageHandling._
        constructMessage(registers, text).map(output => this.copy(output = output)).map(next)
      case _ => Left(s"ERROR: instruction $instruction not yet implemented!")
    }
  }
}

object Program {
  import InstructionHandling._

  def apply(lines: Seq[String]): Program = {
    val instructions = (for (line <- lines; cleaned <- cleanLine(line)) yield cleaned).toIndexedSeq
    new Program(instructions = instructions)
  }

  def next(pgm: Program): Program = pgm.copy(instructionPointer = pgm.instructionPointer + 1)

  private def getIntFrom(registers: Registers, reg: String): Either[String, Int] =
    registers.get(reg) match {
      case Some(x) => Right(x)
      case None => Left(s"register '$reg' does not exist!")
    }

  private def getIntFrom(string: String): Either[String, Int] =
    string.toIntOption match {
      case Some(y) => Right(y)
      case None => Left(s"value '$string' is not an integer!")
    }

  private def getInt(registers: Registers, value: String): Either[String, Int] =
    getIntFrom(value).orElse(getIntFrom(registers, value))

  private def update(registers: Registers, reg: String, value: Int): Registers =
    registers.concat(Map(reg -> value))
}