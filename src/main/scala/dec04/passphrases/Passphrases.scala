package dec04.passphrases

import scala.io.Source

object Passphrases {

  def isValid(str: String): Boolean = {
    val words = str.split("\\s")
    words.size == words.toSet.size
  }

  def countValidPassphrases(lines: Seq[String]) = lines.count(isValid)

  def main(args: Array[String]) = {
    val lines: Seq[String] = Source.fromFile("input").getLines().toSeq

    System.out.println(countValidPassphrases(lines))

  }
}
