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

  def walk(steps: Seq[Step]): (CubeCoordinate, Int) = {
    steps.foldLeft((CubeCoordinate(0, 0, 0), Int.MinValue)){ case ((location, maxDist), step) =>
      val newLocation = location + step
      val dist  = distanceFromOrigin(newLocation)
      (newLocation, Math.max(dist, maxDist))
    }
  }

  def distanceFromOrigin(location: CubeCoordinate): Int = {
    Math.max(Math.max(Math.abs(location.x), Math.abs(location.y)), Math.abs(location.z))
  }

  def calculateSolution(input: String): (Int, Int) = {
    val steps: Seq[Step] = input.split(",").map(parseToStep)
    val finalLocation = walk(steps)
    println("Final location " + finalLocation)
    val distance = distanceFromOrigin(finalLocation._1)
    (distance, finalLocation._2)
  }

  def main(args: Array[String]): Unit = {
    while(true) {
      val input = scala.io.StdIn.readLine("Input: ")
      val (distance, max) = calculateSolution(input)
      println("Distance: ", distance)
      println("Max: ", max)
    }

  }
}

