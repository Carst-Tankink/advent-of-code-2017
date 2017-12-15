package dec15.generators

class Generator(prev: Long, factor: Long, criterion: Int) {
  val modulus: Long = 2147483647

  def next(): (Long, Generator) = {
    val nextVal = (prev * factor) % modulus

    val generator = new Generator(nextVal, factor, criterion)

    if (nextVal % criterion == 0)
      (nextVal, generator)
    else
      generator.next()
  }
}

case class GenA(seed: Long) extends Generator(seed, 16807, 1)

case class GenB(seed: Long) extends Generator(seed, 48271, 1)

object Generators {

  def scorePair(left: Long, right: Long): Int = {
    val mask = 0xFFFF
    if ((left & mask) == (right & mask)) 1 else 0
  }

  def judge(gen1: Generator, gen2: Generator): Long = {
    def rec(steps: Int, score: Int, genA: Generator, genB: Generator): Int = {
      if (steps == 0) score
      else {
        val (nextA, nextGenA) = genA.next()
        val (nextB, nextGenB) = genB.next()
        rec(steps - 1, score + scorePair(nextA, nextB), nextGenA, nextGenB)
      }
    }

    rec(40000000, 0, gen1, gen2)
  }

  def main(args: Array[String]): Unit = {
    println("Input a:")
    val inputA = scala.io.StdIn.readLong()
    println("Input b:")
    val inputB = scala.io.StdIn.readLong()

    val start = System.currentTimeMillis()
    val score = judge(GenA(inputA), GenB(inputB))
    val end = System.currentTimeMillis()
    println("Run time: " + (end - start))

    println("Score: " + score)
  }
}
