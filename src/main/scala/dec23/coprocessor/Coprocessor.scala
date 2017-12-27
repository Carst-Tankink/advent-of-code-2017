package dec23.coprocessor

import scala.io.Source

sealed trait Instruction
case class Set(x: String, y: String) extends Instruction
case class Sub(x: String, y: String) extends Instruction
case class Mul(x: String, y: String) extends Instruction
case class Jnz(x: String, y: String) extends Instruction

case class State(register: Map[String, Int],  pos: Int, muls: Int) {
  def incMul: State = copy(muls = muls + 1)

  def value(y: String): Int = {
    try {
      y.toInt
    } catch {
      case (_: NumberFormatException) => register.getOrElse(y, 0)
    }
  }

  def set(x: String, value: Int): State = copy(register.updated(x, value))
  def jump(steps: Int): State = copy(pos = pos + steps)

}

object Coprocessor {

  def parseInstruction(s: String): Instruction = {
    val tokens = s.split(" ")
    val x = tokens(1)
    val y = tokens(2)
    tokens.head.toLowerCase() match {
      case "set" => Set(x, y)
      case "sub" => Sub(x, y)
      case "mul" => Mul(x, y)
      case "jnz" => Jnz(x, y)
    }
  }

  def run(initialState: State, instr: List[Instruction]): State = {
    def rec(s: State): State = {
      if (s.pos < 0 || s.pos >= instr.size) s
      else {
        val newState =instr(s.pos) match {
          case Set(x, y) => s.set(x, s.value(y)).jump(1)
          case Sub(x, y) => s.set(x, s.value(x) - s.value(y)).jump(1)
          case Mul(x, y) => s.set(x, s.value(x) * s.value(y)).incMul.jump(1)
          case Jnz(x, y) => {
            val xV = s.value(x)
            if (xV != 0) s.jump(s.value(y))
            else s.jump(1)
          }
        }

        rec(newState)
      }
    }

    rec(initialState)
  }

  def runDirect(): Int = {
    List.range(109900, 126901, 17).map(n => BigInt(n)).count(!_.isProbablePrime(100))
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input").getLines().map(parseInstruction).toList
    val finalState = run(State(Map.empty, 0, 0), input)

    println("Muls: " + finalState.muls)

    val finalStateTwo = runDirect()

    println("In h: " + finalStateTwo)

  }
}
