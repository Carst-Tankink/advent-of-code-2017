package dec13.firewall

import scala.io.Source

sealed trait Dir

case class Layer(depth: Int, range: Int) {
  def caught(delay: Int): Boolean = {
    (depth + delay) % (2 * range - 2) == 0
  }
}


object Firewall {

  def toLayer(str: String): Layer = {
    val tokens = str.split(": ").map(_.toInt)
    Layer(tokens.head, tokens.tail.head)
  }

  def passThrough(layers: List[Layer]): Int = {
    layers.filter(_.caught(0)).map(l => l.depth * l.range).sum
  }


  def calculateSafe(layers: List[Layer]) : Int = {
    Stream.from(0).filter(i => layers.forall(l => !l.caught(i))).head
  }

  def main(args: Array[String]): Unit = {
    val layers: List[Layer] = Source.fromFile("input").getLines.map(toLayer).toList
    val severity = passThrough(layers)
    println("Severity: " + severity)

    val safe = calculateSafe(layers)
    println("Safe delay: " + safe)
  }
}
