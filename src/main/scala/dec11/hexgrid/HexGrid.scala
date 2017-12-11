package dec11.hexgrid

sealed class Step(val vector: CubeCoordinate)

case object N extends Step(CubeCoordinate(0, 1, -1))

case object NE extends Step(CubeCoordinate(1, 0, -1))

case object SE extends Step(CubeCoordinate(1, -1, 0))

case object S extends Step(CubeCoordinate(0, -1, 1))

case object SW extends Step(CubeCoordinate(-1, 0, 1))

case object NW extends Step(CubeCoordinate(-1, 1, 0))

case class CubeCoordinate(x: Int, y: Int, z: Int) {
  def +(step: Step): CubeCoordinate = {
    val vector = step.vector
    copy(x + vector.x, y + vector.y, z + vector.z)
  }
}


object HexGrid {
  def parseToStep(v: String): Step = v.toLowerCase match {
    case "n" => N
    case "ne" => NE
    case "se" => SE
    case "s" => S
    case "sw" => SW
    case "nw" => NW
  }

  def walk(steps: Seq[Step]): CubeCoordinate = {
    steps.foldLeft(CubeCoordinate(0, 0, 0))((location, step) => location + step)
  }

  def distanceFromOrigin(location: CubeCoordinate): Int = {
    Math.max(Math.max(Math.abs(location.x), Math.abs(location.y)), Math.abs(location.z))
  }

  def calculateDistance(input: String): Int = {
    val steps: Seq[Step] = input.split(",").map(parseToStep)
    val finalLocation = walk(steps)
    println("Final location " + finalLocation)
    val distance = distanceFromOrigin(finalLocation)
    distance
  }

  def main(args: Array[String]): Unit = {
    while(true) {
      val input = scala.io.StdIn.readLine("Input: ")
      val distance: Int = calculateDistance(input)
      println("Distance: ", distance)
    }

  }
}

