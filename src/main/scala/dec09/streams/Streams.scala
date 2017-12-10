package dec09.streams

import scala.io.StdIn

case class Stream(parent: Option[Stream], children: List[Stream]) {
  def addedChild(s: Stream): Stream = {
    Stream(parent, s :: children)
  }

  override def toString: String = {
    "{" + children.mkString + "}"
  }
}

case class ParserState(current: Option[Stream], garbage: Boolean, canceling: Boolean)

object Streams {


  def nextState(state: ParserState, c: Char): ParserState =
    if (state.canceling) ParserState(state.current, garbage = state.garbage, canceling = false)
    else if (state.garbage) c match {
      case '>' => ParserState(state.current, garbage = false, canceling = state.canceling)
      case '!' => ParserState(state.current, garbage = state.garbage, canceling = true)
      case _ => state
    }
    else c match {
      case '{' =>
        val stream = Stream(state.current, List.empty)
        ParserState(Some(stream), state.garbage, state.canceling)
      case '}' =>
        val parent = state.current.flatMap(_.parent).map(p => p.addedChild(state.current.get))
        ParserState(parent, state.garbage, state.canceling)
      case '<' => ParserState(state.current, garbage = true, state.canceling)
      case _ => state
    }

  def parseStream(input: String): Option[Stream] = {
    val parsed = input.foldLeft(ParserState(Some(Stream(None, List.empty)), garbage = false, canceling = false))((state, c) => nextState(state, c))
    parsed.current
  }

  def calculateScore(s: Stream): Int = {
    def rec(currentScore: Int, s: Stream): Int = {
      val childScores: Int = s.children.map(child => rec(currentScore + 1, child)).sum
      currentScore + childScores
    }

    rec(1, s)
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Input")
      val input: String = StdIn.readLine()
      val stream: Stream = parseStream(input).get.children.head
      println("Stream: " + stream)

      val score = calculateScore(stream)
      println("Score: " + score)
    }
  }
}

