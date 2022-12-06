package aoc2022.day6

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val examples = Map(
      "mjqjpqmgbljsphdztnvjfqwrcgsmlb" -> 7,
      "bvwbjplbgvbhsrlpgdmjqwftvncz" -> 5,
      "nppdvjthqldpwncqszvftbrmjlhg" -> 6,
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" -> 10,
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" -> 11
    )
    val p1 = startMarker(_, 4)

    examples.foreach { case (input, expected) =>
      assert(p1(input) == expected)
    }

    val input = Source.fromResource("2022/6/p1.txt").getLines().next()
    println(p1(input))

    val p2Examples = Map(
      "mjqjpqmgbljsphdztnvjfqwrcgsmlb" -> 19,
      "bvwbjplbgvbhsrlpgdmjqwftvncz" -> 23,
      "nppdvjthqldpwncqszvftbrmjlhg" -> 23,
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" -> 29,
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" -> 26
    )
    val p2 = startMarker(_, 14)

    p2Examples.foreach { case (input, expected) =>
      assert(p2(input) == expected)
    }
    println(p2(input))
  }

  private def startMarker(input: String, width: Int): Int = input.sliding(width).indexWhere(_.toSet.size == width) + width
}
