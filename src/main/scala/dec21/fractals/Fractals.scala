package dec21.fractals

import scala.io.Source

case class Block(content: List[List[Boolean]]) {
  override def toString: String = content.map(l => l.map(x => if (x) "#" else ".").mkString).mkString("/")


  def rotate(b: Block): Block = Block(b.content.transpose.map(_.reverse))

  def variants: Set[Block] = {
    def rotations(x: Block) = {
      val rot1 = rotate(x)
      val rot2 = rotate(rot1)
      val rot3 = rotate(rot2)
      val rotations = Set(x, rot1, rot2, rot3)
      rotations
    }

    val mirror = Block(this.content.reverse)
    rotations(this) ++ rotations(mirror)
  }
}

case class Rule(input: Block, output: Block) {
  override def toString: String = input + " => " + output


  def matches(in: Block): Boolean = {
    true
  }
}

object Fractals {
  def toBlock(input: String): Block = {
    val c = input.trim.split('/').map(row => row.map(c => if (c == '.') false else true).toList).toList
    Block(c)
  }

  def toRules(l: String): Set[Rule] = {
    val tokens = l.split("=>").toList
    val output = tokens(1)
    toBlock(tokens.head).variants.map(i => {
      Rule(i, toBlock(output))
    })
  }

  def expandLines(lines: List[List[Boolean]], divisor: Int): List[List[List[Boolean]]] = {
    val columns = lines.transpose.grouped(divisor).foldLeft(List.empty)((blocks, group) => {
      blocks
    })
    List.empty
  }

  def expand(pic: List[List[Boolean]]): List[List[Boolean]] = {
    val divisor = if (pic.length % 2 == 0) 2 else 3
    pic.grouped(divisor).foldLeft(List.empty)((blocks, lines: List[List[Boolean]]) => {
      val x = expandLines(lines, divisor)
      blocks
    })
    pic
  }

  def generateFractal(rules: Map[Block, Rule], target: Int): List[List[Boolean]] = {
    def rec(pic: List[List[Boolean]], i: Int): List[List[Boolean]] = {
      if (i == target) pic
      else rec(expand(pic), i + 1)
    }

    val start = List(
      List(false, true, false),
      List(false, false, true),
      List(true, true, true)
    )


    rec(start, 0)
  }

  def main(args: Array[String]): Unit = {
    val ruleMap: Map[Block, Rule] = Source.fromFile("input").getLines()
      .flatMap(toRules).toSet
      .groupBy((x: Rule) => x.input)
      .mapValues(r => r.head)
    val picture = generateFractal(ruleMap, 5)

  }
}
