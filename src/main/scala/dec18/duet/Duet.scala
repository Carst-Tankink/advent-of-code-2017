package dec18.duet

import scala.io.Source

case class State(register: Map[String, Long], lastPlayed: Option[Long], position: Long, done: Boolean) {
  private def dereference(y: String): Long = {
    try {
      y.toLong
    } catch {
      case (_: NumberFormatException) => register.getOrElse(y, 0)
    }
  }

  def updated(reg: String, v: Long): State = {
    copy(register + (reg -> v), position = position + 1)
  }

  def get(r: String): Long = {
    dereference(r)
  }
}

sealed trait Instruction {
  def apply(state: State): State
}

case class Snd(x: String) extends Instruction {
  override def apply(state: State): State = State(state.register, Some(state.get(x)), state.position + 1, done = false)
}

case class Set(x: String, y: String) extends Instruction {
  override def apply(state: State): State = state.updated(x, state.get(y))
}

case class Add(x: String, y: String) extends Instruction {
  override def apply(state: State): State = state.updated(x, state.get(x) + state.get(y))
}

case class Mul(x: String, y: String) extends Instruction {
  override def apply(state: State): State = state.updated(x, state.get(x) * state.get(y))
}

case class Mod(x: String, y: String) extends Instruction {
  override def apply(state: State): State = state.updated(x, state.get(x) % state.get(y))
}

case class Rcv(x: String) extends Instruction {
  override def apply(state: State): State = {
    if (state.get(x) != 0) state.copy(done = true) else state.copy(position = state.position + 1)
  }
}

case class Jgz(x: String, y: String) extends Instruction {
  override def apply(state: State): State = {
    val jump = if (state.get(x) > 0) state.get(y) else 1
    state.copy(position = state.position + jump)
  }
}

object Duet {

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
      else rec(instructions(s.position.toInt)(s))
    }

    rec(State(Map.empty, None, 0, done = false))
  }

  def main(args: Array[String]): Unit = {
    val instructions: List[Instruction] = Source.fromFile("input").getLines().map(parseInstruction).toList

    val finalState: State = play(instructions)
    println("Last played: " + finalState.lastPlayed.get)
  }
}
