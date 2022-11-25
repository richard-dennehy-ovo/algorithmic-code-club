package aoc2017.day14

object Main {
  def main(args: Array[String]): Unit = {
    assert(usedSquares("flqrgnkx") == 8108)
    println(usedSquares("hxtvlmkl"))
  }

  def usedSquares(input: String): Int = {
    (0 to 127)
      .map(i => s"$input-$i")
      .map(knotHash)
      .flatMap(values => values.map(_.toBinaryString.count(_ == '1')))
      .sum
  }

  def knotHash(input: String): Vector[Int] = {
    val lengths =
      input.iterator.filterNot(_.isWhitespace).map(_.toInt).toVector ++ Vector(
        17,
        31,
        73,
        47,
        23
      )
    val state =
      Iterator.continually(lengths).take(64).flatten.foldLeft(HashState()) {
        case (state, step) => state.next(step)
      }
    state.denseHash
  }

  case class HashState(values: Vector[Int], position: Int, skip: Int) {
    def next(length: Int): HashState = {
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

      HashState(newValues, newPosition, newSkip)
    }

    def denseHash: Vector[Int] =
      values.grouped(16).map(_.reduce(_ ^ _)).toVector
  }

  object HashState {
    def apply(): HashState = HashState((0 until 256).toVector, 0, 0)
  }
}
