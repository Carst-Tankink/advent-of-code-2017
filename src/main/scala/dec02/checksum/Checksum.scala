package dec02.checksum

import scala.io.Source

object Checksum {

  def parseLine(line: String): Seq[Int] = {
    line.split("\\s").map(x => x.toInt)
  }


  def diffMaxMin(ints: Seq[Int]): Int = {
    val maxMin: (Int, Int) = ints.foldRight((Int.MinValue, Int.MaxValue))((value, acc) => {
      val newMax = if (value >= acc._1) value else acc._1
      val newMin = if (value <= acc._2) value else acc._2
      (newMax, newMin)
    })

    maxMin._1 - maxMin._2
  }


  def divideEvenlyDivisible(ints: Seq[Int]): Int = {
    val (x1, x2) : (Int, Int) = ints.combinations(2)
      .map(els => (els.head, els.tail.head))
      .filter { case (e1, e2) => e1 % e2 == 0 || e2 % e1  == 0}
      .toSeq
      .head

    if (x1 > x2) x1 / x2 else x2 / x1
  }

  def main(args: Array[String]): Unit = {
    val lines: Seq[Seq[Int]] = Source.fromFile("input").getLines().toSeq.map(parseLine)

    val solution2: Int = lines.map(divideEvenlyDivisible).sum
    println(solution2)

  }

}
