package dec04.passphrases

import scala.io.Source

object Passphrases {

  def noDuplicates(str: String): Boolean = {
    val words = str.split("\\s")
    words.size == words.toSet.size
  }

  def countValidPassphrases(lines: Seq[String]): Int = lines.count(noDuplicates)

  def countOccurrences(word: String): Map[Char, Int] = word.foldLeft(Map[Char, Int]())((acc, c) => acc + (c -> (acc.getOrElse(c, 0) + 1)))

  def isAnagram(word1: String, word2: String): Boolean = {
    val result = countOccurrences(word1) == countOccurrences(word2)

    result
  }

  def hasAnagram(word: String, words: Seq[String]): Boolean = {
    words.count(isAnagram(word, _)) > 1
  }


  def noAnagrams(str: String): Boolean = {
    val words = str.split("\\s")
    words.length == words.count(w => !hasAnagram(w, words))
  }

  def countAnagramValid(lines: Seq[String]): Int = lines.count(noAnagrams)

  def main(args: Array[String]) = {
    val lines: Seq[String] = Source.fromFile("input").getLines().toSeq

    System.out.println(countValidPassphrases(lines))
    System.out.println(countAnagramValid(lines))

  }
}
