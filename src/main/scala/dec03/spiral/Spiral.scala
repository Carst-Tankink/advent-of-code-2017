package dec03.spiral

object Spiral {

  case class CartesianCoordinate(x: Int, y: Int) {
    def >(bound: Int): Boolean = Math.abs(x) > bound || Math.abs(y) > bound

    def add(other: CartesianCoordinate): CartesianCoordinate = {
      CartesianCoordinate(this.x + other.x, this.y + other.y)
    }

    override def toString: String = "(" + x + "," + y + ")"
  }

  sealed class Direction(val vector: CartesianCoordinate)

  case object Up extends Direction(CartesianCoordinate(0, 1))

  case object Left extends Direction(CartesianCoordinate(-1, 0))

  case object Down extends Direction(CartesianCoordinate(0, -1))

  case object Right extends Direction(CartesianCoordinate(1, 0))

  def next(dir: Direction): Direction = dir match {
    case Up => Left
    case Left => Down
    case Down => Right
    case Right => Up
    case x => x
  }

  def manhattanDistance(p1: CartesianCoordinate, p2: CartesianCoordinate): Int = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  def updateDirection(location: CartesianCoordinate, dir: Direction, bound: Int): Direction = {
    val outOfBounds = location.add(dir.vector) > bound
    if (outOfBounds) next(dir) else dir
  }

  def makeGrid(spiralCoordinate: Int, current: Int, grid: Map[Int, CartesianCoordinate], direction: Direction, bound: Int): Map[Int, CartesianCoordinate] = {
    val location: CartesianCoordinate = grid.getOrElse(current - 1, CartesianCoordinate(0, 0))
    val newLocation = location.add(direction.vector)
    if (current >= spiralCoordinate) grid + (current -> newLocation)
    else {
      val newBound = updateBound(newLocation, bound)
      makeGrid(spiralCoordinate, current + 1,
        grid + (current -> newLocation),
        updateDirection(newLocation, direction, newBound),
        newBound)
    }
  }

  def updateBound(location: CartesianCoordinate, bound: Int): Int = {
    val isLowerCorner = location == CartesianCoordinate(bound, -bound)
    if (isLowerCorner) bound + 1 else bound
  }

  def spiralToCartesian(spiralCoordinate: Int): CartesianCoordinate = {
    makeGrid(spiralCoordinate, 1, Map(0 -> CartesianCoordinate(-1, 0)), Right, 1).get(spiralCoordinate).orNull
  }

  def fillGrid(spiralCoordinate: Int): Map[CartesianCoordinate, Long] = {
    def fillRec(location: Int, grid: Map[Int, CartesianCoordinate], filledGrid: Map[CartesianCoordinate, Long]): Map[CartesianCoordinate, Long] = {
      if (location > spiralCoordinate) {
        filledGrid
      }
      else {
        val cart = grid.getOrElse(location, CartesianCoordinate(0, 0))
        val left = cart.add(Left.vector)
        val bottomLeft = left.add(Down.vector)
        val bottom = bottomLeft.add(Right.vector)
        val bottomRight = bottom.add(Right.vector)
        val right = bottomRight.add(Up.vector)
        val topRight = right.add(Up.vector)
        val top = topRight.add(Left.vector)
        val topLeft = top.add(Left.vector)

        val lVal: Long = filledGrid.getOrElse(left, 0)
        val blVal: Long = filledGrid.getOrElse(bottomLeft, 0)
        val bVal: Long = filledGrid.getOrElse(bottom, 0)
        val brVal: Long = filledGrid.getOrElse(bottomRight, 0)
        val rVal: Long = filledGrid.getOrElse(right, 0)
        val trVal: Long = filledGrid.getOrElse(topRight, 0)
        val tVal: Long = filledGrid.getOrElse(top, 0)
        val tlVal: Long = filledGrid.getOrElse(topLeft, 0)


        val newVal: Long = if (location == 1) 1 else lVal + blVal + bVal + brVal + rVal + trVal + tVal + tlVal

        fillRec(location + 1, grid, filledGrid + (cart -> newVal))
      }
    }

    val grid: Map[Int, CartesianCoordinate] = makeGrid(spiralCoordinate, 1, Map(0 -> CartesianCoordinate(-1, 0)), Right, 1)

    fillRec(1, grid, Map())

  }

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Input: ")
      val input = scala.io.StdIn.readInt()
      val solution = manhattanDistance(CartesianCoordinate(0, 0), spiralToCartesian(input))
      println(solution)


      val grid = fillGrid(input + 1)
      var i = 1
      var output: Long = 0
      while(output < input) {
        output = grid.getOrElse(spiralToCartesian(i), 0)
        i += 1
      }
      println(output)
    }
  }
}
