package aoc2017.day5

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val jumps =
      Source.fromResource("2017/5/part1.txt").getLines().map(_.toInt).toVector
    val initial = State(jumps, 0)
    val Some((_, steps)) = Iterator.iterate(initial)(_.part2Next).zipWithIndex.find {
      case (state, _) => state.position >= state.jumps.length
    }
    println(s"steps: $steps")
  }
}

case class State(jumps: Vector[Int], position: Int) {
  def part1Next: State = {
    val jump = jumps(position)
    State(jumps.updated(position, jump + 1), position + jump)
  }

  def part2Next: State = {
    val jump = jumps(position)
    val updated = if (jump >= 3) {
      jump - 1
    } else {
      jump + 1
    }
    State(jumps.updated(position, updated), position + jump)
  }
}
