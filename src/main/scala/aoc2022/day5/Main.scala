package aoc2022.day5

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """    [D]
                    |[N] [C]
                    |[Z] [M] [P]
                    | 1   2   3
                    |
                    |move 1 from 2 to 1
                    |move 3 from 1 to 3
                    |move 2 from 2 to 1
                    |move 1 from 1 to 2""".stripMargin

    assert(p1(example.linesIterator) == "CMZ")
    assert(p2(example.linesIterator) == "MCD")

    val input = Source.fromResource("2022/5/p1.txt").getLines().toVector
    println(p1(input.iterator))
    println(p2(input.iterator))
  }

  def p1(input: Iterator[String]): String = {
    val stacks = parseStacks(input)
    val instructions = input.map(Instruction.parse).toVector

    val finalState = instructions.foldLeft(State(stacks)) { case (state, instruction) =>
      state.nextP1(instruction)
    }
    finalState.message
  }

  def p2(input: Iterator[String]): String = {
    val stacks = parseStacks(input)
    val instructions = input.map(Instruction.parse).toVector

    val finalState = instructions.foldLeft(State(stacks)) { case (state, instruction) =>
      state.nextP2(instruction)
    }
    finalState.message
  }

  case class State(stacks: Vector[Vector[Char]]) {
    def nextP1(instruction: Instruction): State = {
      val (moved, left) = stacks(instruction.from).splitAt(instruction.count)
      val target = stacks(instruction.to)

      State(
        stacks
          .updated(instruction.from, left)
          .updated(instruction.to, moved.reverse ++ target)
      )
    }

    def nextP2(instruction: Instruction): State = {
      val (moved, left) = stacks(instruction.from).splitAt(instruction.count)
      val target = stacks(instruction.to)

      State(
        stacks
          .updated(instruction.from, left)
          .updated(instruction.to, moved ++ target)
      )
    }

    def message: String = stacks.map(_.head).mkString
  }

  def parseStacks(input: Iterator[String]): Vector[Vector[Char]] = {
    input.takeWhile(_.nonEmpty).foldLeft(Vector.empty[Vector[Char]]) {
      case (stacks, line) =>
        line.grouped(4).zipWithIndex.foldLeft(stacks) {
          case (stacks, (crate, index)) =>
            if (crate.isBlank) {
              stacks
            } else if (crate.startsWith("[")) {
              if (stacks.length <= index) {
                val padded = stacks.padTo(index + 1, Vector.empty)
                val stack = padded(index)
                padded.updated(index, stack.appended(crate(1)))
              } else {
                val stack = stacks(index)
                stacks.updated(index, stack.appended(crate(1)))
              }
            } else {
              // stack number - shouldn't be needed
              stacks
            }
        }
    }
  }

  case class Instruction(count: Int, from: Int, to: Int)
  object Instruction {
    def parse(line: String): Instruction = {
      val regex = raw"move (\d+) from (\d+) to (\d+)".r
      line match {
        case regex(count, from, to) =>
          Instruction(count.toInt, from.toInt - 1, to.toInt - 1)
        case _ => throw new IllegalArgumentException(s"Unparseable: $line")
      }
    }
  }
}
