package captcha

import scala.io.StdIn


object Captcha {

  case class State(acc: Int, prev: Option[Int])

  def parseAsListOfInt(input: String): Seq[Int] = input.map (_.getNumericValue)

  def sumRepeats(ints: Seq[Int]): Int = {
    val circular: Seq[Int] = ints ++ ints.headOption
    val finalState = circular.foldRight(State(0, None))((i: Int, s: State) => {
      val newAcc: Int = s.acc + (if (s.prev.contains(i)) i else 0)
      State(newAcc, Some(i))
    })

    finalState.acc
  }

  def main(args: Array[String]): Unit = {
    val input: String = StdIn.readLine("Input: ")
    val output = sumRepeats(parseAsListOfInt(input))
    println(output)
  }
}
