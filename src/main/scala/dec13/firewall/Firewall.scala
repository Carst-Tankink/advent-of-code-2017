package dec13.firewall

import scala.io.Source

sealed trait Dir

case object Up extends Dir

case object Down extends Dir

case class Layer(scanner: Int, scannerDir: Dir, depth: Int, range: Int) {
  def next: Layer = {
    val updateVal = if (scannerDir == Up) -1 else 1
    val newScanner = scanner + updateVal

    val newDir = if (newScanner == range - 1) Up else if (newScanner == 0) Down else scannerDir
    copy(scanner = newScanner, newDir)
  }
}

case class PassageState(accumulatedSeverity: Int, layers: List[Layer])

object Firewall {

  def toLayer(str: String): Layer = {
    val tokens = str.split(": ").map(_.toInt)
    Layer(0, Down, tokens.head, tokens.tail.head)
  }

  def getSeverity(pos: Int, s: PassageState): Int = {
    s.layers
      .filter(l => l.scanner == 0)
      .filter(l => {
        val depth = l.depth
        depth == pos
      })
      .map(l => {
        l.depth * l.range
      })
      .headOption
      .getOrElse(0)
  }

  def passThrough(layers: List[Layer]): Int = {
    val maxDepth = layers.map(_.depth).max
    List.range(0, maxDepth + 1).foldLeft(PassageState(0, layers))((s, pos) => {
      val severity = getSeverity(pos, s)
      val stepScanners = s.layers.map(l => l.next)
      PassageState(s.accumulatedSeverity + severity, stepScanners)
    }).accumulatedSeverity
  }

  def main(args: Array[String]): Unit = {
    val layers: List[Layer] = Source.fromFile("input").getLines.map(toLayer).toList
    val severity = passThrough(layers)
    println("Severity: " + severity)
  }
}
