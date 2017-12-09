package dec08.registers

import scala.io.Source

sealed trait Operation

case object Inc extends Operation

case object Dec extends Operation

case class Instruction(register: String, op: Operation, amount: Int, condition: Map[String, Int] => Boolean)

object Registers {

  def makeCondition(register: String, cmp: (Int, Int) => Boolean, value: Int): Map[String, Int] => Boolean = {
    registers => cmp(registers.getOrElse(register, 0), value.toInt)
  }

  def parseComparator(token: String): (Int, Int) => Boolean = token match {
    case ">" => (x: Int, y: Int) => x > y
    case "<" => (x: Int, y: Int) => x < y
    case ">=" => (x: Int, y: Int) => x >= y
    case "<=" => (x: Int, y: Int) => x <= y
    case "==" => (x: Int, y: Int) => x == y
    case "!=" => (x: Int, y: Int) => x != y
  }

  def parseInstruction(line: String): Instruction = {
    val tokens: Seq[String] = line.split(' ')
    val register = tokens(0)
    val operation = if (tokens(1).equalsIgnoreCase("inc")) Inc else Dec
    val amount = tokens(2).toInt
    val condition = makeCondition(tokens(4), parseComparator(tokens(5)), tokens(6).toInt)

    Instruction(register, operation, amount, condition)
  }

  def apply(instr: Instruction, registers: Map[String, Int]): Map[String, Int] = {
    if (!instr.condition(registers)) {

      registers
    }
    else {
      val current = registers.getOrElse(instr.register, 0)
      val newValue = current + (if (instr.op == Inc) instr.amount else -instr.amount)
      val newReg = registers.updated(instr.register, newValue)
      newReg

    }
  }

  def runProgram(instructions: Seq[Instruction]): Map[String, Int] = {
    instructions.foldLeft(Map.empty:Map[String, Int]) ((registers, i) => apply(i, registers))
  }

  def main(args: Array[String]): Unit = {
    val strings = Source.fromFile("input").getLines()
    val instructions: Seq[Instruction] = strings.map(parseInstruction).toSeq
    val registers: Map[String, Int] = runProgram(instructions)
    println(registers.maxBy(_._2))
  }
}
