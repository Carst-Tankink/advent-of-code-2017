package dec14.defragmentation

import dec10.hash.Hash

object Defragmentation {

  def toBits(value: String): String = "%4s".format(Integer.toBinaryString(Integer.valueOf(value, 16))).replace(" ", "0")

  def decodeHash(hexString: List[String]): List[Boolean] = {
    hexString.mkString
      .map(c => toBits(c.toString))
      .flatMap(s => s.map(c => if(c == '1') true else false))
      .toList
  }

  def calculateGrid(input: String): scala.List[scala.List[Boolean]] = {
    List.range(0, 128)
      .map(i => input + "-" + i)
      .map(Hash.hashString)
      .map(decodeHash)
  }

  def main(args: Array[String]): Unit = {
    while(true) {
      println("Input: ")
      val input = scala.io.StdIn.readLine()
      val grid: List[List[Boolean]] = calculateGrid(input)

      val used: Int = grid.map(l => l.count(b => b)).sum
      println("Used squares: " + used)

    }
  }
}
