package aoc2022.day4

import scala.io.Source
import scala.util.matching.Regex

object Main {
  def main(args: Array[String]): Unit = {
    val example = """2-4,6-8
                  |2-3,4-5
                  |5-7,7-9
                  |2-8,3-7
                  |6-6,4-6
                  |2-6,4-8""".stripMargin

    assert(p1(example.linesIterator) == 2)
    val input = Source.fromResource("2022/4/p1.txt").getLines().toVector
    println(p1(input.iterator))

    assert(p2(example.linesIterator) == 4)
    println(p2(input.iterator))
  }

  private val pairRegex: Regex = raw"(\d+?)-(\d+?),(\d+?)-(\d+?)".r
  def p1(input: Iterator[String]): Int = {
    input.count { pair =>
      pair match {
        case pairRegex(firstStart, firstEnd, secondStart, secondEnd) =>
          val firstRange = (firstStart.toInt to firstEnd.toInt).toSet
          val secondRange = (secondStart.toInt to secondEnd.toInt).toSet
          firstRange.diff(secondRange).isEmpty || secondRange.diff(firstRange).isEmpty
        case _ => throw new IllegalArgumentException(s"Unexpected input $pair")
      }
    }
  }

  def p2(input: Iterator[String]): Int = {
    input.count { pair =>
      pair match {
        case pairRegex(firstStart, firstEnd, secondStart, secondEnd) =>
          val firstRange = (firstStart.toInt to firstEnd.toInt).toSet
          val secondRange = (secondStart.toInt to secondEnd.toInt).toSet
          firstRange.intersect(secondRange).nonEmpty
        case _ => throw new IllegalArgumentException(s"Unexpected input $pair")
      }
    }
  }
}
