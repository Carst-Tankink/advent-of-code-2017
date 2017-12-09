package dec05.jumps

import scala.io.Source

case class Tape(left: List[Int], head: Option[Int], right: List[Int])


object Jumps {

  def moveLeft(tape: Tape): Tape =
    if (tape.left.isEmpty && tape.head.isEmpty) tape
    else if (tape.left.isEmpty) Tape(tape.left, None, tape.head.get :: tape.right)
    else Tape(tape.left.tail, tape.left.headOption, tape.head.get :: tape.right)

  def moveRight(tape: Tape): Tape =
    if (tape.right.isEmpty && tape.head.isEmpty) tape
    else if (tape.right.isEmpty) Tape(tape.head.get :: tape.left, None, tape.right)
    else
      Tape(tape.head.get :: tape.left, tape.right.headOption, tape.right.tail)

  def move(tape: Tape, jump: Int): Tape = {
    if (jump == 0) tape
    else if (jump < 0) move(moveLeft(tape), jump + 1)
    else move(moveRight(tape), jump - 1)
  }

  def run(instructions: Seq[Int], update: Int => Int): Long = {
    def rec(idx: Int, instr: Tape, steps: Long): Long = {
      if (instr.head.isEmpty) steps
      else {
        val jump = instr.head.get
        val newHead = update(jump)
        val newTape = move(Tape(instr.left, Some(newHead), instr.right), jump)
        rec(idx + jump, newTape, steps + 1)
      }
    }

    val t: Tape = Tape(List.empty, instructions.headOption, instructions.tail.toList)
    rec(0, t, 0)
  }

  def main(args: Array[String]): Unit = {
    val instructions: Seq[Int] = Source.fromFile("input").getLines().map(s => s.toInt).toSeq
    val inc: (Int) => Int = (x: Int) => x + 1

//        val steps = run(instructions, inc)
    val steps = run(instructions, (x: Int) => if (x >= 3) x - 1 else x + 1)
    println(steps)

  }
}
