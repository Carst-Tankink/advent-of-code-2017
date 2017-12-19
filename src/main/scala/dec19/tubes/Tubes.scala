package dec19.tubes

import scala.io.Source

sealed trait Direction {
  def next(x: Int, y: Int): (Int, Int)
}

case object Up extends Direction {
  override def next(x: Int, y: Int): (Int, Int) = (x, y - 1)
}

case object Down extends Direction {
  override def next(x: Int, y: Int): (Int, Int) = (x, y + 1)
}

case object Left extends Direction {
  override def next(x: Int, y: Int): (Int, Int) = (x - 1, y)
}

case object Right extends Direction {
  override def next(x: Int, y: Int): (Int, Int) = (x + 1, y)
}

sealed class Node(val x: Int, val y: Int) {
  def getChar(): Option[Char] = None
}


case class Vertical(override val x: Int, override val y: Int) extends Node(x, y)

case class Horizontal(override val x: Int, override val y: Int) extends Node(x, y)

case class Crossing(override val x: Int, override val y: Int) extends Node(x, y)

case class Letter(override val x: Int, override val y: Int, c: Char) extends Node(x, y) {
  override def getChar() = Some(c)
}

object Tubes {
  def parseLine(y: Int, line: String): List[Node] = {
    line.zipWithIndex.filter(d => !d._1.isWhitespace).map { case (c, x) => c match {
      case '|' => Vertical(x, y)
      case '-' => Horizontal(x, y)
      case '+' => Crossing(x, y)
      case d if !d.isWhitespace => Letter(x, y, d)
    }
    }.toList
  }

  def atEnd(x: Int, y: Int, input: List[List[Node]]): Boolean = {
    input.lift(y).forall(l => l.lift(x).isEmpty)
  }


  def inverse(d: Direction): Direction = d match {
    case Up => Down
    case Down => Up
    case Left => Right
    case Right => Left
  }

  def newDir(oldDir: Direction, p: Crossing, input: Map[(Int, Int), Node]): Direction =
    List(Up, Down, Left, Right)
      .filter(d => d != inverse(oldDir))
      .find(d => {
        input.contains(d.next(p.x, p.y))
      })
      .get

  def walkMaze(input: Map[(Int, Int), Node]): (String, Int) = {
    def step(acc: String, pos: Node, dir: Direction, steps: Int): (String, Int) = {
      val nextDir = pos match {
        case p@Crossing(_, _) => newDir(dir, p, input)
        case _ => dir
      }

      val nextPos = input.get(nextDir.next(pos.x, pos.y))
      if (nextPos.isEmpty) (acc, steps)
      else step(nextPos.get.getChar().map(c => acc + c).getOrElse(acc), nextPos.get, nextDir, steps + 1)
    }

    val start = input.find { case ((_, y), _) => y == 0 }.get._2

    println("Start: " + start)

    step("", start, Down, 1)
  }

  def main(args: Array[String]): Unit = {
    val input: Map[(Int, Int), Node] = Source.fromFile("input").getLines()
      .zipWithIndex
      .flatMap { case (l, index) => parseLine(index, l) }
      .map(n => (n.x, n.y) -> n)
      .toMap

    val (word, steps) = walkMaze(input)
    //    println("Input: \n" + input.mkString("\n"))

    println("Word: " + word)
    println("Steps: " + steps)
  }
}

