package dec22.virus

import scala.io.Source

sealed trait Status

case object Clean extends Status

case object Infected extends Status

sealed class Direction(val vector: (Int, Int))

case object Up extends Direction(0, -1)

case object Right extends Direction(1, 0)

case object Down extends Direction(0, 1)

case object Left extends Direction(-1, 0)

case class Carrier(pos: (Int, Int), dir: Direction, burst: Int, infections: Int, status: Map[(Int, Int), Status]) {
  def updatedStatus: Map[(Int, Int), Status] = {
    val newStatus = if (currentStatus == Clean) Infected else Clean
    status.updated(pos, newStatus)
  }

  def currentStatus: Status = status.getOrElse(pos, Clean)
}

object Virus {
  def turnRight(d: Direction): Direction = d match {
    case Up => Right
    case Right => Down
    case Down => Left
    case Left => Up
  }

  def turnLeft(d: Direction): Direction = d match {
    case Up => Left
    case Left => Down
    case Down => Right
    case Right => Up
  }

  def run(bursts: Int, statusMap: Map[(Int, Int), Status]): Carrier = {
    def rec(carrier: Carrier): Carrier = {
      if (carrier.burst == bursts) carrier
      else {
        val newDir = if (carrier.currentStatus == Infected) turnRight(carrier.dir) else turnLeft(carrier.dir)
        val newStatuses = carrier.updatedStatus
        val newPos = (carrier.pos._1 + newDir.vector._1, carrier.pos._2 + newDir.vector._2)
        val newInfections = carrier.infections + (if (carrier.currentStatus == Infected) 0 else 1)
        val newCarrier = Carrier(newPos, newDir, carrier.burst + 1, newInfections, newStatuses)
        rec(newCarrier)
      }
    }

    rec(Carrier((0, 0), Up, 0, 0, statusMap))
  }

  def parseLine(s: String): List[(Status, Int)] = {
    s.map {
      case '.' => Clean
      case '#' => Infected
    }
      .zipWithIndex
      .toList
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input").getLines()
      .map(parseLine)
      .toList

    val horizontalOffset = (lines.head.size - 1) / 2
    val verticalOffset = (lines.size - 1) / 2

    val statusMap: Map[(Int, Int), Status] = lines
      .zipWithIndex
      .flatMap { case (l, y) => l.map { case (s, x) => (x - horizontalOffset, y - verticalOffset) -> s } }
      .toMap

    val finalStatus = run(10000, statusMap)

    println("Infections: " + finalStatus.infections)

  }
}
