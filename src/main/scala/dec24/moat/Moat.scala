package dec24.moat

import scala.io.Source

case class Component(port1: Int, port2: Int) {
  def other(toConnect: Int): Int = if (port1 == toConnect) port2 else port1

  def strength: Int = port1 + port2
}

case class Bridge(components: List[Component]) {
  def length: Int = components.size

  def strength: Int = components.map(_.strength).sum

  def add(component: Component): Bridge = {
    copy(component :: components)
  }
}

object Moat {
  def buildAllBridges(components: Set[Component]): Set[Bridge] = {
    def rec(toConnect: Int, remaining: Set[Component], currentBridge: Bridge, assembled: Set[Bridge]): Set[Bridge] = {
      val elements = remaining.filter(c => c.port1 == toConnect || c.port2 == toConnect)
      if (elements.isEmpty) assembled + currentBridge
      else {
        elements.flatMap(e => rec(e.other(toConnect), remaining - e, currentBridge.add(e), assembled))
      }

    }

    rec(0, components, Bridge(List.empty), Set.empty)
  }

  def main(args: Array[String]): Unit = {
    val components: Set[Component] = Source.fromFile("input").getLines().map(x => {
      val tokens = x.split("/")
      Component(tokens.head.toInt, tokens(1).toInt)
    }).toSet

    val allBridges = buildAllBridges(components)

    println("Strongest " + allBridges.maxBy(_.strength).strength)
    val longestLength = allBridges.map(_.length).max
    println("Longest: " + allBridges.filter(_.length == longestLength).maxBy(_.strength).strength)
  }
}
