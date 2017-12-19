package dec18.duet

import scala.io.Source

case class Register(private val content: Map[String, Long] = Map.empty) {
  def updated(reg: String, v: Long): Register = {
    copy(content + (reg -> v))
  }

  def get(r: String): Long = {
    try {
      r.toLong
    } catch {
      case (_: NumberFormatException) => content.getOrElse(r, 0)
    }
  }

}

case class State(register: Register, lastPlayed: Option[Long], position: Long, done: Boolean) {
  def get(x: String): Long = register.get(x)

  def updated(x: String, v: Long): State = this.copy(register = register.updated(x, v), position = position + 1)
}

sealed trait Instruction

case class Snd(x: String) extends Instruction

case class Set(x: String, y: String) extends Instruction

case class Add(x: String, y: String) extends Instruction

case class Mul(x: String, y: String) extends Instruction

case class Mod(x: String, y: String) extends Instruction

case class Rcv(x: String) extends Instruction

case class Jgz(x: String, y: String) extends Instruction

object Duet {
  def music(instruction: Instruction, state: State): State = instruction match {
    case Snd(x) => State(state.register, Some(state.get(x)), state.position + 1, done = false)
    case Set(x, y) => state.updated(x, state.get(y))
    case Add(x, y) => state.updated(x, state.get(x) + state.get(y))
    case Mul(x, y) => state.updated(x, state.get(x) * state.get(y))
    case Mod(x, y) => state.updated(x, state.get(x) % state.get(y))
    case Rcv(x) => if (state.get(x) != 0) state.copy(done = true) else state.copy(position = state.position + 1)
    case Jgz(x, y) =>
      val jump = if (state.get(x) > 0) state.get(y) else 1
      state.copy(position = state.position + jump)
  }

  def parseInstruction(s: String): Instruction = {
    val tokens = s.split(' ')
    val register = tokens(1)
    tokens(0) match {
      case "snd" => Snd(register)
      case "set" => Set(register, tokens(2))
      case "add" => Add(register, tokens(2))
      case "mul" => Mul(register, tokens(2))
      case "mod" => Mod(register, tokens(2))
      case "rcv" => Rcv(register)
      case "jgz" => Jgz(register, tokens(2))
    }
  }

  def play(instructions: List[Instruction]): State = {
    def rec(s: State): State = {
      if (s.done) s
      else {
        val position = s.position.toInt
        if (position < 0 || position > instructions.length) {
          s.copy(done = true)
        }
        else {
          rec(music(instructions(position), s))
        }
      }
    }

    rec(State(Register(), None, 0, done = false))
  }

  def main(args: Array[String]): Unit = {
    val instructions: List[Instruction] = Source.fromFile("input").getLines().map(parseInstruction).toList

    val finalState: State = play(instructions)
    println("Last played: " + finalState.lastPlayed.get)
  }
}
