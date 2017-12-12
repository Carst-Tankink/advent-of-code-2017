package dec12.pipes

import scala.io.Source

case class Pipe(from: Int, to: Set[Int]) {
  override def toString: String = from + " <-> " + to.mkString(", ")
}

object Pipes {
  def parseToPipe(line: String): Pipe = {
    val tokens: List[String] = line.split("<->")
      .map(_.trim)
      .toList

    val source = tokens.head.toInt
    val targets: Set[Int] = tokens(1).split(',').map(x => x.trim.toInt).toSet

    Pipe(source, targets)
  }


  def fillTransitive(f: Int, m: Map[Int, Set[Int]]): Set[Int] = {
    def rec(from: Int, visited: Set[Int]): Set[Int] = {
      if (visited.contains(from)) visited
      else m.getOrElse(from, Set.empty).foldLeft(visited + from)((v, e) => v ++ rec(e, v))
    }

    rec(f, Set.empty)
  }

  def findReachable(pipes: List[Pipe]): Map[Int, Set[Int]] = {
    val initial: Map[Int, Set[Int]] = pipes.map(p => p.from -> p.to).toMap

    pipes.foldLeft((initial, Set.empty[Int])) { case ((m, visited), p) => {
      val key = p.from
      if (visited.contains(key)) (m, visited)
      else {
        val values = fillTransitive(key, m)
        (m ++ values.map(v => v -> values), visited ++ values)
      }
    }
    }._1
  }

  def getGroups(reachable: Map[Int, Set[Int]]): Set[Set[Int]] = reachable.values.toSet

  def main(args: Array[String]): Unit = {
    val pipes: List[Pipe] = Source.fromFile("input").getLines().map(parseToPipe).toList
    val reachable: Map[Int, Set[Int]] = findReachable(pipes)
    println("From 0: " + reachable.getOrElse(0, Set.empty).size)
    val groups: Set[Set[Int]] = getGroups(reachable)
    println("Groups size: " + groups.size)
  }
}
