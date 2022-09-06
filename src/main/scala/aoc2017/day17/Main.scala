package aoc2017.day17

import java.time.Instant
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println(s"example: ${bruteForcePart1(3)}")
    println(s"part 1: ${bruteForcePart1(380)}")

    assert(part2(2017, 3) == 1226)
    assert(part2(2017, 380) == 665)
    println(s"part 2: ${part2(50 * 1000 * 1000, 380)}")
  }

  private def bruteForcePart1(increment: Int): Int = {
    val filled = (1 to 2017 foldLeft State(Vector(0), 0)) { case (State(buffer, index), nextValue) =>
      if (nextValue % 10000 == 0) println(s"${Instant.now}: $nextValue")
      val nextIndex = ((index + increment) % buffer.size) + 1
      val (front, back) = buffer.splitAt(nextIndex)
      State(front.appended(nextValue) ++ back, nextIndex)
    }
    val lastInserted = filled.buffer.indexOf(2017)
//    println(filled.buffer)
    filled.buffer((lastInserted + 1) % filled.buffer.size)
  }

  case class State(buffer: Vector[Int], index: Int)

  private def part2(iterations: Int, increment: Int): Int = {
    @tailrec def valueInSecond(lastWrittenTo: Int, current: Int, value: Int): Int = if (current > iterations) value else {
      val target = (lastWrittenTo + increment) % current + 1
      if (target == 1) {
        valueInSecond(target, current + 1, current)
      } else {
        valueInSecond(target, current + 1, value)
      }
    }

    valueInSecond(0, 1, 1)
  }
}
