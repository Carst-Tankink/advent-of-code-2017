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

  def runProgram(instructions: Seq[Instruction]): (Int, Map[String, Int]) = {
    instructions.foldLeft((Int.MinValue, Map.empty:Map[String, Int])) ((maxAndRegisters, i) => {
      val newRegister = apply(i, maxAndRegisters._2)
      val max = if (newRegister.isEmpty) maxAndRegisters._1 else Math.max(newRegister.maxBy(_._2)._2, maxAndRegisters._1)
      (max, newRegister)
    })
  }

  def main(args: Array[String]): Unit = {
    val strings = Source.fromFile("input").getLines()
    val instructions: Seq[Instruction] = strings.map(parseInstruction).toSeq
    val result: (Int,Map[String, Int]) = runProgram(instructions)
    println(result._2.maxBy(_._2))
    println(result._1)
  }
}
