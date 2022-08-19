package aoc2017.day12

import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = Source.fromResource("2017/12/example.txt").getLines()

    @tailrec
    def expand(connections: Map[Int, Set[Int]], seen: Set[Int], next: Set[Int]): Set[Int] = {
      if (seen ++ next == seen) {
        seen
      } else {
        expand(connections, seen ++ next, next.flatMap(connections))
      }
    }

    val exampleConnections = connections(example)
    val expanded = expand(exampleConnections, Set.empty, exampleConnections(0))
    assert(expanded.size == 6)

    val part1 = Source.fromResource("2017/12/part1.txt").getLines()
    val part1Connections = connections(part1)
    val expandedPart1 = expand(part1Connections, Set.empty, part1Connections(0))
    println(s"part1: ${expandedPart1.size}")

    @tailrec def groups(remaining: Map[Int, Set[Int]], acc: Vector[Set[Int]]): Vector[Set[Int]] = {
      if (remaining.isEmpty) {
        acc
      } else {
        val unreached = remaining.head._2
        val expanded = expand(remaining, Set.empty, unreached)
        groups(remaining -- expanded, acc.appended(expanded))
      }
    }

    val remainingGroups = groups(part1Connections -- expandedPart1, Vector.empty)
    println(s"part2: ${remainingGroups.length + 1}")
  }

  def connections(input: Iterator[String]): Map[Int, Set[Int]] = {
    val connectionRegex = "(.*?) <-> (.*)".r
    input.map {
      case connectionRegex(node, children) => node.toInt -> children.split(",").map(_.trim.toInt).toSet
    }.toMap
  }
}
