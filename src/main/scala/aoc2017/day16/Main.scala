package aoc2017.day16

import scala.io.Source
import scala.language.postfixOps

object Main {
  def main(args: Array[String]): Unit = {
    val exampleInstructions = "s1,x3/4,pe/b"
    val exampleMoves = exampleInstructions.split(",").map(Move.parse)
    val exampleInput = 'a' to 'e' toVector
    val exampleResult = dance(exampleInput, exampleMoves)
    assert(exampleResult.mkString == "baedc")

    val instructions = Source.fromResource("2017/16/part1.txt").mkString.trim
    val moves = instructions.split(",").map(Move.parse)
    val part1Input = 'a' to 'p' toVector
    val part1 = dance(part1Input, moves)
    println(s"part1: ${part1.mkString}")

    val cycleLength = Iterator.iterate(part1Input)(dance(_, moves)).indexOf(part1Input, 1)
    val remainder = (1000 * 1000 * 1000) % cycleLength
    val part2 = Iterator.iterate(part1Input)(dance(_, moves)).drop(remainder).next().mkString
    println(s"part2: $part2")
  }

  private def dance(initial: Vector[Char], moves: Array[Move]): Vector[Char] = {
    moves.foldLeft(initial) { case (state, move) =>
      move match {
        case Move.Spin(steps) =>
          val (first, second) = state.splitAt(state.length - steps)
          second ++ first
        case Move.Exchange(first, second) =>
          state
            .updated(first, state(second))
            .updated(second, state(first))
        case Move.Swap(first, second) =>
          state
            .updated(state.indexOf(first), second)
            .updated(state.indexOf(second), first)
      }
    }
  }

  sealed trait Move
  object Move {
    case class Spin(steps: Int) extends Move
    case class Exchange(first: Int, second: Int) extends Move
    case class Swap(first: Char, second: Char) extends Move

    def parse(instruction: String): Move = {
      instruction.head match {
        case 's' => Spin(instruction.tail.toInt)
        case 'x' =>
          val split = instruction.tail.split('/')
          assert(split.length == 2)
          Exchange(split.head.toInt, split(1).toInt)
        case 'p' =>
          assert(instruction.length == 4)
          assert(instruction(2) == '/')
          Swap(instruction(1), instruction.last)
        case _ =>
          throw new IllegalArgumentException(
            s"Invalid instruction $instruction"
          )
      }
    }
  }
}
