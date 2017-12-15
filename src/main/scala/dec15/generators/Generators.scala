package dec15.generators

import java.lang.Long.toBinaryString

class Generator(prev: Long, factor: Long) {
  val modulus: Long = 2147483647

  def next(): (Long, Generator) = {
    val nextVal = (prev * factor) % modulus
    (nextVal, new Generator(nextVal, factor))
  }
}

case class GenA(seed: Long) extends Generator(seed, 16807)

case class GenB(seed: Long) extends Generator(seed, 48271)

object Generators {

  def scorePair(left: Long, right: Long): Int = {
    if (toBinaryString(left).takeRight(16).equals(toBinaryString(right).takeRight(16))) 1 else 0
  }

  def judge(gen1: Generator, gen2: Generator): Long = {
    val steps = 40000000
    List.range(0, steps).foldLeft(0, gen1, gen2) { case ((score, genA, genB), _) =>
      val (nextA, nextGenA) = genA.next()
      val (nextB, nextGenB) = genB.next()

      (score + scorePair(nextA, nextB), nextGenA, nextGenB)
    }
  }._1

  def main(args: Array[String]): Unit = {

    val testA = GenA(65)
    val testB = GenB(8921)

    val start = System.currentTimeMillis()

    println("Test score: " + judge(testA, testB))

    val end = System.currentTimeMillis()

    println("Run time: " + (end - start))

    println("Input a:")
    val inputA = scala.io.StdIn.readLong()
    println("Input b:")
    val inputB = scala.io.StdIn.readLong()


    println("Score: " + judge(GenA(inputA), GenB(inputB)))
  }
}
