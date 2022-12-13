package aoc2022.day10

import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
//    val trivialExample = """noop
//                           |addx 3
//                           |addx -5""".stripMargin
//    val trivialInstructions = trivialExample
//      .linesIterator
//      .map(Instruction.parse)
//      .toVector
//

    val actualExample = Source.fromResource("2022/10/example.txt").getLines().map(Instruction.parse).toVector
    println(runP1(actualExample))

    val input = Source.fromResource("2022/10/p1.txt").getLines().map(Instruction.parse).toVector
    println(runP1(input))
  }

  def runP1(instructions: Vector[Instruction]): Int = {
    @tailrec def loop(xs: Vector[Int], remaining: Vector[Instruction]): Vector[Int] = {
      val x = xs.last

      remaining.headOption match {
        case None => xs
        case Some(NoOp) => loop(xs :+ x, remaining.tail)
        case Some(AddX(value)) => loop(xs ++ Vector(x, x + value), remaining.tail)
      }
    }

    loop(Vector(1), instructions).zipWithIndex.collect { case (x, cycleZeroIndexed) if (cycleZeroIndexed - 19) % 40 == 0 => x * (cycleZeroIndexed + 1) }.sum
  }

  sealed trait Instruction
  case object NoOp extends Instruction
  case class AddX(value: Int) extends Instruction

  object Instruction {
    def parse(line: String): Instruction = {
      if (line == "noop") {
        NoOp
      } else if (line.startsWith("addx ")) {
        AddX(line.drop("addx ".length).toInt)
      } else {
        throw new IllegalArgumentException(s"Unparsable: $line")
      }
    }
  }
}
