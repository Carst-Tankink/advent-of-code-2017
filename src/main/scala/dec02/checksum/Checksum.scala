package dec02.checksum

import scala.io.Source

object Checksum {

  def parseLine(line: String): Seq[Int] = {
    line.split("\\s").map(x => x.toInt)
  }


  def diffMaxMin(ints: Seq[Int]): Int = {
    val maxMin: (Int, Int) = ints.foldRight ((Int.MinValue, Int.MaxValue)) ((value, acc) => {
      val newMax = if (value >= acc._1) value else acc._1
      val newMin = if (value <= acc._2) value else acc._2
      (newMax, newMin)
    })

    maxMin._1 - maxMin._2
  }


  def main(args: Array[String]): Unit = {
    val spreadSheet: Seq[String] = Source.fromFile("input").getLines().toSeq
    val lines: Seq[Seq[Int]] = spreadSheet.map(parseLine)

    val solution: Int =  lines.map(diffMaxMin).sum

    println(solution)
  }

}
