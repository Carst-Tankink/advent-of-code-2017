package dec17.spinlock

case class SpinState(afterZero: Long, position: Long, length: Long)

case class ListZipper(left: List[Int], right: List[Int]) {
  def insert(i: Int): ListZipper = {
    copy(left = left, right = right.head :: i :: right.tail)
  }

  def step(): ListZipper = {
    val newZ = copy(left = right.head :: left, right = right.tail)
    if (newZ.right.isEmpty) {
      ListZipper(List.empty, newZ.left.reverse)
    } else {
      newZ
    }
  }

  def steps(i: Int): ListZipper = {
    if (i == 0) this
    else this.step().steps(i - 1)
  }

  def currentValue: Int = {
    right.head
  }
}

object Spinlock {

  def insertValues(stepSize: Int): ListZipper = {
    val initial = ListZipper(List.empty, List(0))
    List.range(1, 2018).foldLeft(initial)((zipper, i) => {
      val zipper1 = zipper.steps(stepSize).insert(i)
      zipper1.step()
    })
  }


  def calculateSpin(stepSize: Int): Long = {
    Stream.range(1, 50000001).foldLeft(SpinState(0, 0, 1))((state, i) => {
      val newPosition = (state.position + stepSize) % state.length
      val afterZero = if (newPosition == 0) i else state.afterZero

      SpinState(afterZero, newPosition + 1, state.length + 1)
    }).afterZero
  }

  def main(args: Array[String]): Unit = {
    println("Input:")
    val stepSize = scala.io.StdIn.readInt()
    val finalList = insertValues(stepSize)
    println("Value after head: " + finalList.step().currentValue)

    val spinLockFirst = calculateSpin(stepSize)
    println("Value after steps: " + spinLockFirst)
  }
}
