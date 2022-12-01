package aoc2022.day1

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example =
      """1000
        |2000
        |3000
        |
        |4000
        |
        |5000
        |6000
        |
        |7000
        |8000
        |9000
        |
        |10000""".stripMargin.linesIterator
    assert(largest3(example).sum == 45000)

    val p1 = Source.fromResource("2022/1/p1.txt").getLines()
    println(largest3(p1).sum)
  }

  def largest3(values: Iterator[String]): List[Int] = {
    val totals = values.foldLeft(List(0)) { case (head :: tail, value) =>
      if (value.isEmpty) {
        0 :: head :: tail
      } else {
        head + value.toInt :: tail
      }
    }
    totals.sorted.takeRight(3)
  }
}
