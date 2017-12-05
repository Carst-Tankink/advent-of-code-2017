package captcha

import scala.io.StdIn

object SkipCaptcha extends {

  def skipCaptcha(ints: Seq[Int]): Int = {
    val halfway: Int = ints.length / 2
    (ints.take(halfway) zip ints.takeRight(halfway)).foldRight (0) ((p: (Int, Int), acc: Int) => {
      acc + (if (p._1 == p._2) p._1 + p._2 else 0)
    })
  }

  def main(args: Array[String]): Unit = {
    while(true) {
      val input: String = StdIn.readLine("Input: ")
      val output = skipCaptcha(Captcha.parseAsListOfInt(input))
      println(output)
    }
  }

}