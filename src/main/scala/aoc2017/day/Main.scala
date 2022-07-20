package aoc2017.day

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val examples = Map(
      "{}" -> 1,
      "{{{}}}}" -> 6,
      "{{},{}}" -> 5,
      "{{{},{},{{}}}}" -> 16,
      "{<a>,<a>,<a>,<a>}" -> 1,
      "{{<ab>},{<ab>},{<ab>},{<ab>}}" -> 9,
      "{{<!!>},{<!!>},{<!!>},{<!!>}}" -> 9,
      "{{<a!>},{<a!>},{<a!>},{<ab>}}" -> 3,
    )

    examples.foreach { case (example, score) =>
      assert(example.foldLeft(State.init)(_.next(_)).totalScore == score)
    }

    val input = Source.fromResource("2017/9/part1.txt").to(LazyList).foldLeft(State.init)(_.next(_))
    println(s"part1: ${input.totalScore}")

    val p2Examples = Map(
      "<>" -> 0,
      "<random characters>" -> 17,
      "<<<<>" -> 3,
      "<{!>}>" -> 2,
      "<!!>" -> 0,
      "<!!!>>" -> 0,
      "<{o\"i!a,<{i<a>" -> 10,
    )

    p2Examples.foreach { case (example, garbage) =>
      assert(example.foldLeft(State.init)(_.next(_)).totalGarbage == garbage)
    }

    println(s"part2: ${input.totalGarbage}")
  }
}

case class State(
  totalScore: Int,
  totalGarbage: Int,
  depth: Int,
  ignoreNext: Boolean,
  inGarbage: Boolean,
) {
  def next(char: Char): State = {
    char match {
      case _ if ignoreNext => this.copy(ignoreNext = false)
      case '!' => this.copy(ignoreNext = true)
      case '>' if inGarbage => this.copy(inGarbage = false)
      case '<' if !inGarbage => this.copy(inGarbage = true)
      case '{' if !inGarbage => this.copy(depth = this.depth + 1)
      case '}' if !inGarbage => this.copy(totalScore = this.totalScore + this.depth, depth = this.depth - 1)
      case _ if inGarbage => this.copy(totalGarbage = this.totalGarbage + 1)
      case _ => this
    }
  }
}

object State {
  def init: State = State(
    0,
    0,
    0,
    false,
    false,
  )
}
