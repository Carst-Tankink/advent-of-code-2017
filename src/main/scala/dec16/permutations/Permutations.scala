package dec16.permutations

sealed trait Step {
  def apply(input: String): String
}

case class Spin(number: Int) extends Step {
  override def apply(input: String): String = {
    val (left, right) = input.splitAt(input.length - number)
    right ++ left
  }
}

case class Exchange(pos1: Int, pos2: Int) extends Step {
  override def apply(input: String): String = {
    val c1 = input(pos1)
    val c2 = input(pos2)
    input.updated(pos1, c2).updated(pos2, c1)
  }
}

case class Partner(char1: Char, char2: Char) extends Step {
  override def apply(input: String): String = {
    val pos1 = input.indexOf(char1)
    val pos2 = input.indexOf(char2)
    input.updated(pos1, char2).updated(pos2, char1)
  }
}


object Permutations {

  def parseStep(s: String): Step = s.head match {
    case 's' => Spin(Integer.parseInt(s.tail))
    case 'x' =>
      val tokens = s.tail.split('/')
      Exchange(Integer.parseInt(tokens(0)), Integer.parseInt(tokens(1)))
    case 'p' => Partner(s(1), s(3))
  }

  val startString: String = List.range(0, 16).map(x => (x+ 97).toChar).mkString
  def dance(start: String, steps: Seq[Step]): String = {
    steps.foldLeft(start)((order, step) => step(order))
  }

  def repeatDance(steps: Seq[Step]): String = {
    def rec(stepsLeft: Long, input: String) : String = {
      if (stepsLeft == 0) input
      else rec(stepsLeft -1, dance(input, steps))
    }

    rec(1000000000L, startString)
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Input: ")
      val steps: Seq[Step] = scala.io.StdIn.readLine().split(",").map(s => parseStep(s))
      val finalOrder = dance(startString, steps)
      println("Final " + finalOrder)
      val partTwoFinal = repeatDance(steps)
      println("Final after one billion: " + partTwoFinal)
    }
  }
}
