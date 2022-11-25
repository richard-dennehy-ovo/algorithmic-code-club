package aoc2017.day10

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val exampleInput = Vector(3, 4, 1, 5)
    val example = exampleInput.foldLeft(State(5)) { case (state, step) => state.next(step) }
    println(example.values(0) * example.values(1))

    val p1Input = Source.fromResource("2017/10/part1.txt").getLines().flatMap(_.split(",").map(_.toInt))
    val p1 = p1Input.foldLeft(State(256)) { case (state, step) => state.next(step) }
    println(p1.values(0) * p1.values(1))

    val p2Input = Source.fromResource("2017/10/part1.txt").filterNot(_.isWhitespace).map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)
    val p2State = Iterator.continually(p2Input).take(64).flatten.foldLeft(State(256)) { case (state, step) => state.next(step) }
    println(p2State.denseHash.map(_.toHexString.prepended('0').takeRight(2)).mkString)
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

    def denseHash: Vector[Int] = values.grouped(16).map(_.reduce(_ ^ _)).toVector
  }

  object State {
    def apply(size: Int): State = State((0 until size).toVector, 0, 0)
  }
}
