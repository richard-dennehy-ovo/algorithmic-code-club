package aoc2017.day10

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val exampleInput = Vector(3, 4, 1, 5)
    val example = exampleInput.foldLeft(State((0 to 4).toVector, 0, 0)) { case (state, step) => state.next(step) }
    println(example.values(0) * example.values(1))

    val input = Source.fromResource("2017/10/part1.txt").getLines().flatMap(_.split(",").map(_.toInt))
    val p1 = input.foldLeft(State((0 to 255).toVector, 0, 0)) { case (state, step) => state.next(step) }
    println(p1.values(0) * p1.values(1))
  }

  case class State(values: Vector[Int], position: Int, skip: Int) {
    def next(length: Int): State = {
      val newValues = if (position + length >= values.length) {
        // take values from end, append wraparound values to create reversal list
        // reverse list
        // take wraparound from reversed list, append untouched values, append rest of reversed list
        val wraparound = (position + length) % values.length
        val (head, tail) = values.splitAt(position)
        val (wrapped, rest) = head.splitAt(wraparound)
        val reversed = (tail ++ wrapped).reverse
        reversed.takeRight(wraparound) ++ rest ++ reversed.dropRight(wraparound)
      } else {
        val (head, tail) = values.splitAt(position)
        val (section, rest) = tail.splitAt(length)
        head ++ section.reverse ++ rest
      }
      val newPosition = (position + length + skip) % values.length
      val newSkip = skip + 1

      State(newValues, newPosition, newSkip)
    }
  }
}
