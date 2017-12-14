package dec14.defragmentation

import dec10.hash.Hash

case class Region(id: Int, members: Set[(Int, Int)])

object Defragmentation {

  def toBits(value: String): String = "%4s".format(Integer.toBinaryString(Integer.valueOf(value, 16))).replace(" ", "0")

  def decodeHash(hexString: List[String]): List[Boolean] = {
    hexString.mkString
      .map(c => toBits(c.toString))
      .flatMap(s => s.map(c => if (c == '1') true else false))
      .toList
  }

  def calculateGrid(input: String): scala.List[scala.List[Boolean]] = {
    List.range(0, 128)
      .map(i => input + "-" + i)
      .map(Hash.hashString)
      .map(decodeHash)
  }


  def mergeRegions(region1: Region, region2: Region): Region = {
    val lowestId = Math.min(region1.id, region2.id)

    Region(lowestId, region1.members.union(region2.members))
  }

  def regionsOfList(y: Int, line: List[Boolean], regions: (Map[(Int, Int), Region], Int)): (Map[(Int, Int), Region], Int) = {
    line.zipWithIndex
      .filter(_._1)
      .map(_._2)
      .foldLeft(regions) { case ((reg, maxId), x) =>
        val regionLeft = reg.get((x - 1, y))
        val regionUp = reg.get((x, y - 1))
        val region = Region(maxId + 1, Set((x, y)))

        val leftAndRegion = regionLeft.map(r => mergeRegions(r, region)).getOrElse(region)
        val merged = regionUp.map(r => mergeRegions(r, leftAndRegion)).getOrElse(leftAndRegion)

        (merged.members.foldLeft(reg)((newRegions, coords) => newRegions + (coords -> merged)), Math.max(merged.id, maxId))
      }
  }

  def calculateRegions(grid: List[List[Boolean]]): Map[(Int, Int), Region] = {
    grid.zipWithIndex
      .foldLeft((Map.empty[(Int, Int), Region], 0))((regions, entry) =>
        regionsOfList(entry._2, entry._1, regions))._1
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Input: ")
      val input = scala.io.StdIn.readLine()
      val grid: List[List[Boolean]] = calculateGrid(input)

      val used: Int = grid.map(l => l.count(b => b)).sum
      println("Used squares: " + used)

      val regions = calculateRegions(grid)
      println("Number of regions: "+ regions.values.toSet.size)
    }
  }
}