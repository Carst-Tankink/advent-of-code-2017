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
    println("State: " + this + " length: " + length)
    val sublist = getSublist(state, currentPosition, currentPosition + length)
    val newState = inject(state, sublist.reverse, currentPosition)
    println("Newstate: " + newState)
    val newPosition = (currentPosition + length + skipSize) % state.size
    copy(state = newState, currentPosition = newPosition, skipSize = skipSize + 1)
  }
}

object Hash {

  def hashFunction(input: Seq[Int]): Seq[Int] = {
    val seed: List[Int] = List.range(0, 256)
    input
      .filter(l => l <= seed.length)
      .foldLeft(HashState(seed, 0, 0))((state, length) => state.update(length)).state
  }

  def main(args: Array[String]): Unit = {
    println("Input: ")
    val input: Seq[Int] = scala.io.StdIn.readLine().split(",").map(_.toInt)
    val hash = hashFunction(input)
    println("Hash: ", hash)
    val checkSum = hash.head * hash.tail.head
    println("Checksum: " + checkSum)
  }
}

