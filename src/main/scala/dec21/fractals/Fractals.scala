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

object Fractals {
  def toBlock(input: String): Block = {
    val c = input.trim.split('/').map(row => row.map(c => if (c == '.') false else true).toList).toList
    Block(c)
  }

  def toRules(l: String): Set[(Block, Block)] = {
    val tokens = l.split("=>").toList
    val output = tokens(1)
    toBlock(tokens.head).variants.map(i => {
      (i, toBlock(output))
    })
  }

  def concatBlocks(line: List[Block]): Block = {
    def rec(l: List[List[List[Boolean]]], acc: List[List[Boolean]]): List[List[Boolean]] =
    if (l.exists(_.isEmpty)) acc
    else {3
      val heads: List[Boolean] = l.flatMap(_.head)
      val tails: List[List[List[Boolean]]] = l.map(_.tail)
      rec(tails, acc ++ List(heads))
    }

    Block(rec(line.map(_.content), List.empty))
  }

  def divide(b: Block): List[List[Block]] = {
    val divisor = if (b.content.size % 2 == 0) 2 else 3
    b.content
      .grouped(divisor)
      .map((g: List[List[Boolean]]) => {
        val parts: List[List[List[Boolean]]] = g.map(l => l.grouped(divisor).toList)
        if (divisor == 2) {
          val tops = parts.head
          val bottoms = parts.tail.head
          (tops zip bottoms).map(p => Block(List(p._1, p._2)))
        } else {
          val tops = parts.head
          val middles = parts(1)
          val bottoms = parts(2)
          (tops zip middles zip bottoms).map(p => Block(List(p._1._1, p._1._2, p._2)))
        }
      })
      .toList
  }

  def expand(pic: Block, rules: Map[Block, Block]): Block = {
    val div = divide(pic)

    val expanded: List[List[Block]] = div.map(l => l.map(rules(_)))

    val mergedPerLine: List[Block] = expanded.map((line: List[Block]) => concatBlocks(line))
    Block(mergedPerLine.flatMap(b => b.content))
  }

  def generateFractal(rules: Map[Block, Block], target: Int): Block = {
    def rec(pic: Block, i: Int): Block = {
      if (i == target) pic
      else rec(expand(pic, rules), i + 1)
    }

    val start = Block(List(
      List(false, true, false),
      List(false, false, true),
      List(true, true, true)
    ))


    rec(start, 0)
  }

  def main(args: Array[String]): Unit = {
    val ruleMap: Map[Block, Block] = Source.fromFile("input").getLines()
      .flatMap(toRules)
      .toMap
    val picture = generateFractal(ruleMap, 5)

    println("On: " + picture.content.flatMap(l => l.map(b => if (b) 1 else 0)).sum)
    val eighteen = generateFractal(ruleMap, 18)
    println("On: " + eighteen.content.flatMap(l => l.map(b => if (b) 1 else 0)).sum)
  }
}
