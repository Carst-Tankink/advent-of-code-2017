package dec05.jumps

import scala.io.Source

object Jumps {
  def run(instructions: Seq[Int], update: Int => Int): Long = {
    def rec(idx: Int, instr: Seq[Int], steps: Long): Long = {
      if (idx < 0 || idx >= instr.length) steps
      else {
        val jump = instr(idx)
        val newInstr = instr.updated(idx, update(jump))
        rec(idx + jump, newInstr, steps + 1)
      }
    }
    rec(0, instructions, 0)
  }

  def main(args: Array[String]): Unit = {
    val instructions: Seq[Int] = Source.fromFile("input").getLines().map(s => s.toInt).toSeq
    val inc: (Int) => Int = (x: Int) => x + 1

//    val steps = run(instructions, inc)
    val steps =  run(instructions, (x: Int) => if(x >= 3) x-1 else x+1)
    println(steps)

  }
}
