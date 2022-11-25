package aoc2017.day2

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("2017/2/part1.txt")
      .getLines()
      .map(_.split("\\s").map(_.toInt))
      .toVector
    val p1 = input.map { values =>
      val (min, max) = values.foldLeft(Int.MaxValue -> 0) {
        case ((min, max), value) => math.min(min, value) -> math.max(max, value)
      }
      max - min
    }.sum
    println(p1)

    val p2 = input.map { values =>
      val pairs = for {
        v1 <- values
        v2 <- values if v2 != v1 && v1 % v2 == 0
      } yield (v1, v2)
      assert(pairs.length == 1)
      val (v1, v2) = pairs(0)
      v1 / v2
    }.sum
    println(p2)
  }
}
