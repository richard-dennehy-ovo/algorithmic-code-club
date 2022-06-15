package aoc2017.day4

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val part1 = Source.fromResource("2017/4/part1.txt").getLines()

    val valid = part1.count { p =>
      val sortedWords = p.split(" ").map(word => word.toVector.sorted.mkString)
      sortedWords.distinct.length == sortedWords.length
    }
    println(s"valid $valid")
  }
}
