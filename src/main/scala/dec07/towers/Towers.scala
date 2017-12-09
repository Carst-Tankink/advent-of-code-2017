package dec07.towers

import scala.io.Source

case class ProgramDescription(name: String, weight: Int, supports: List[String])

case class Program(name: String, weight: Int, supports: List[Program])

object Towers {

  def parseLine(str: String): ProgramDescription = {
    val tokens = str.split("\\s")
    val name = tokens(0)
    val weight = tokens(1).substring(1, tokens(1).length - 1).toInt

    val supports = if (tokens.length > 2)
      tokens.drop(3).map(n => n.stripSuffix(",")).toList
    else List.empty

    ProgramDescription(name, weight, supports)
  }

  def findRoot(input: Seq[ProgramDescription]): ProgramDescription = {
    val parentMap: Map[String, ProgramDescription] =
      input.foldRight(Map.empty[String, ProgramDescription])((pd, map) => {
        val parent: Option[ProgramDescription] = input.find(p => p.supports.contains(pd.name))
        if(parent.isEmpty) map
        else map + (pd.name -> parent.get)
      }
    )

    input.find(p => !parentMap.contains(p.name)).get
  }

  def main(args: Array[String]): Unit = {
    val input: Seq[ProgramDescription] = Source.fromFile("input").getLines().map(parseLine).toSeq
    val support = findRoot(input)

    print(support.name)
  }
}

