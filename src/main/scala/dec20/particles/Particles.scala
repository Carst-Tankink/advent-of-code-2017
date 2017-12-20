package dec20.particles

import scala.io.Source

case class Vector(x: Long, y: Long, z: Long) {
  def manhattanDistance(pos: Vector): Long = {
    Math.abs(x - pos.x) + Math.abs(y - pos.y) + Math.abs(z - pos.z)
  }

  def manhattanValue: Long = Math.abs(x) + Math.abs(y) + Math.abs(z)

  def +(v: Vector): Vector = {
    Vector(x + v.x, y + v.y, z + v.z)
  }
}

case class Particle(id: Int, pos: Vector, velocity: Vector, acceleration: Vector) {
  def move(): Particle = {
    val newV = velocity + acceleration
    copy(pos = pos + newV, velocity = newV)
  }
}

object Particles {

  def parseVec(str: String): Vector = {
    val vec = str.split('=')(1).replace("<", "").replace(">", "").split(",")
    Vector(vec(0).toInt, vec(1).toInt, vec(2).toInt)
  }

  def parseParticle(i: Int, l: String): Particle = {
    val elems = l.split(">,").map(_.trim)
    val p = parseVec(elems(0))
    val v = parseVec(elems(1))
    val a = parseVec(elems(2))
    Particle(i, p, v, a)
  }

  def simulateCollisions(input: List[Particle], steps: Int): List[Particle] = {
    val withoutCollisions: List[Particle] = removeCollisions(input)
    if (steps == 10000) withoutCollisions
    else {
      val next = withoutCollisions.map(_.move())
      simulateCollisions(next, steps + 1)
    }
  }


  private def removeCollisions(input: List[Particle]): List[Particle] = {
    val vectorToParticles = input
      .groupBy(_.pos)

    vectorToParticles
      .values
      .filter(_.size == 1)
      .flatten
      .toList
  }

  def main(args: Array[String]): Unit = {
    val input: List[Particle] = Source.fromFile("input").getLines().zipWithIndex.map(p => parseParticle(p._2, p._1)).toList

    val minAcc = input.map(x => x.acceleration.manhattanValue).min
    val slowest = input.filter(p => p.acceleration.manhattanValue == minAcc)
    println("Slowest: " + slowest)

    val remainder = simulateCollisions(input, 0)
    println("Remainder: " + remainder.size)
  }
}
