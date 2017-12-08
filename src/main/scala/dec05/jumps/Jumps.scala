package dec05.jumps

import scala.io.Source

object Jumps {


  def run(instructions: Seq[Int]): Int = {
    def rec(idx: Int, instr: Seq[Int], steps: Int): Int = {
      if (idx < 0 || idx >= instr.length) steps
      else {
        val jump = instr(idx)
        val newInstr = instr.updated(idx, jump + 1)
        rec(idx + jump, newInstr, steps + 1)
      }
    }
    rec(0, instructions, 0)
  }

  def main(args: Array[String]): Unit = {
    val instructions: Seq[Int] = Source.fromFile("input").getLines().map(s => s.toInt).toSeq
    val steps = run(instructions)

    println(steps)

  }
}
