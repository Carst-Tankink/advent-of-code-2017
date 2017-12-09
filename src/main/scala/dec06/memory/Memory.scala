package dec06.memory

object Memory {
  def parseAsListOfInt(input: String): Seq[Int] = input.split("\\s+").filter(_.nonEmpty).map(_.toInt)

  def updateBank(bank: Seq[Int], i: Int, highest: Int): scala.Seq[Int] = {
    if (highest == 0) bank
    else updateBank(bank.updated(i, bank(i) + 1), (i + 1) % bank.length, highest - 1)
  }


  def reallocate(bank: Seq[Int]): Seq[Int] = {
    val (highest, index): (Int, Int) = bank.zipWithIndex.maxBy(p => p._1)

    updateBank(bank.updated(index, 0), (index + 1) % bank.length, highest)
  }

  def cycle(input: Seq[Int]): (Map[Seq[Int], Seq[Int]], Seq[Int]) = {
    def rec(bank: Seq[Int], seen: Map[Seq[Int], Seq[Int]]): (Map[Seq[Int], Seq[Int]], Seq[Int]) = {
      if (seen.contains(bank)) (seen, bank)
      else {
        val newBank = reallocate(bank)
        rec(newBank, seen + (bank -> newBank))
      }
    }

    rec(input, Map.empty)
  }

  def findCycle(graph: Map[Seq[Int], Seq[Int]], startAt: Seq[Int]): Int = {
    def rec(current: Seq[Int], steps: Int): Int = {
      if (current == startAt) steps
      else rec(graph(current), steps + 1)
    }

    rec(graph(startAt), 1)
  }

  def main(args: Array[String]): Unit = {
    while(true) {
      println("Input:")
      val input = parseAsListOfInt(scala.io.StdIn.readLine())
      val solution = cycle(input)
      println("Solution:")
      println(solution._1.size)
      val cycleSize = findCycle(solution._1, solution._2)
      println("Cycle size: ")
      println(cycleSize)
    }

  }
}
