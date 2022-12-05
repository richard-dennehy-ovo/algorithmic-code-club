package aoc2022.day3

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """vJrwpWtwJgWrhcsFMMfFFhFp
                    |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                    |PmmdzqPrVvPwwTWBwg
                    |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                    |ttgJtRGJQctTZtZT
                    |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

    assert(p1(example.linesIterator) == 157)

    val input =
      Source.fromResource("2022/3/p1.txt").getLines().filterNot(_.isEmpty).toVector
    println(p1(input.iterator))

    assert(p2(example.linesIterator) == 70)
    println(p2(input.iterator))
  }

  def p1(input: Iterator[String]): Int = {
    input.map { line =>
      val (first, second) = line.splitAt(line.length / 2)
      val shared = Set.from(first).intersect(Set.from(second))
      assert(shared.size == 1)
      priority(shared.head)
    }.sum
  }

  def p2(input: Iterator[String]): Int = {
    input
      .sliding(3, 3)
      .map { group =>
        val first = group.head
        val second = group(1)
        val third = group(2)
        val shared =
          Set.from(first).intersect(Set.from(second)).intersect(Set.from(third))
        assert(shared.size == 1)
        priority(shared.head)
      }
      .sum
  }

  private def priority(char: Char): Int = {
    if (char >= 'a') {
      char.toInt - 96
    } else {
      char.toInt - 38
    }
  }
}
