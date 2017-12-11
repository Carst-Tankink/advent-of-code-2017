package dec10.hash

case class HashState(state: List[Int], currentPosition: Int, skipSize: Int) {

  def getSublist(state: List[Int], start: Int, end: Int): List[Int] = {
    val fromBeginning = if (end > state.length) end % state.length else 0
    val beginning: List[Int] = state.take(fromBeginning)

    state.slice(start, end) ++ beginning
  }

  def inject(state: List[Int], sublist: List[Int], start: Int): List[Int] = {
    if (sublist.isEmpty) state
    else inject(state.updated(start, sublist.head), sublist.tail, (start + 1) % state.length)
  }

  def update(length: Int): HashState = {
    val sublist = getSublist(state, currentPosition, currentPosition + length)
    val newState = inject(state, sublist.reverse, currentPosition)
    val newPosition = (currentPosition + length + skipSize) % state.size
    copy(state = newState, currentPosition = newPosition, skipSize = skipSize + 1)
  }
}

object Hash {

  def hashFunction(input: Seq[Int], inState: HashState): HashState = {
    input
      .filter(l => l <= inState.state.length)
      .foldLeft(inState)((state, length) => state.update(length))
  }

  def runRounds(input: List[Int]): HashState = {
    def round(roundNumber: Int, inputState: HashState): HashState = {
      if (roundNumber == 64) inputState
      else round(roundNumber + 1, hashFunction(input, inputState))
    }

    val seed = List.range(0, 256)
    round(0, HashState(seed, 0, 0))
  }

  def makeDense(sparseHash: HashState) : List[Int] = {
    sparseHash.state.grouped(16).map(group => group.reduceLeft((x, y) => x ^ y)).toList
  }

  private def hashString(inputString: String) = {
    val input: Seq[Int] = inputString.map(_.toInt)
    val padded = input ++ List(17, 31, 73, 47, 23)

    val sparseHash = runRounds(padded.toList)
    val denseHash = makeDense(sparseHash)

    val hex = denseHash.map((x: Int) => "%02x".format(x))
    hex
  }

  def main(args: Array[String]): Unit = {
    println("Input: ")
    val inputString = scala.io.StdIn.readLine()
    val hash: List[String] = hashString(inputString)
    println("hash: " + hash.mkString)
    }
}

