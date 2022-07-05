package aoc2017.day6

import scala.annotation.tailrec

object Main {
  private val example = Memory(Vector(0, 2, 7, 0))
  private val actualInput = Memory(
    Vector(5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6)
  )

  def main(args: Array[String]): Unit = {
    @tailrec
    def acc(prev: Vector[Memory], next: Memory): Vector[Memory] = {
      if (prev.contains(next)) {
        prev.appended(next)
      } else {
        val redist = next.redistribute
        assert(next.banks.sum == redist.banks.sum)
        acc(prev.appended(next), redist)
      }
    }
    val steps = acc(Vector.empty, actualInput)
    println(s"part 1: ${steps.size - 1}")
    val dupeFirstSeen = steps.indexOf(steps.last)
    println(s"part 2: ${steps.size - 1 - dupeFirstSeen}")
  }
}

case class Memory(banks: Vector[Int]) {
  val largest: Int = banks.zipWithIndex
    .foldLeft((0, 0)) {
      case (acc @ (accSize, accIndex), elem @ (size, index)) =>
        if (size > accSize) {
          elem
        } else if (accSize == size && index < accIndex) {
          elem
        } else {
          acc
        }
    }
    ._2

  def redistribute: Memory = {
    val blocks = banks(largest)
    val each = blocks / banks.size
    val remainder = blocks % banks.size
    val wrapIndex = (largest + remainder) % banks.size

    Memory(
      banks.zipWithIndex.map { case (size, index) =>
        if (index == largest) {
          each
        } else if (wrapIndex < largest && (index > largest || index <= wrapIndex)) {
          size + each + 1
        } else if (wrapIndex > largest && index > largest && index <= wrapIndex) {
          size + each + 1
        } else {
          size + each
        }
      }
    )
  }
}
