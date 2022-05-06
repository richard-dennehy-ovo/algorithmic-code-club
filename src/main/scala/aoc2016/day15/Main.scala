package aoc2016.day15

import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    println(s"Example answer: ${example()}")
    println(s"part 1 answer: ${part1()}")
    println(s"part 2 answer: ${part2()}")
  }

  private def example(): Int = {
    val discs = Vector(
      Disc(5, 4, 1),
      Disc(2, 1, 2),
    )

    calculateTime(discs)
  }

  private def part1(): Int = {
    val input =
      Source
        .fromResource("day 15 part 1.txt")
        .getLines()
        .map(Disc.parse)
        .toVector

    val time = calculateTime(input)
    assert(time == 121834, time)
    time
  }

  private def part2(): Int = {
    val input =
      Source
        .fromResource("day 15 part 1.txt")
        .getLines()
        .map(Disc.parse)
        .toVector

    val time = calculateTime(input.appended(Disc(11, 0, input.length + 1)))
    assert(time == 3208099, time)
    time
  }

  private def calculateTime(discs: Vector[Disc]): Int = {
    val (first, rest) = (discs.head, discs.tail)
    val (stepsTaken, _) = rest.foldLeft((first.stepsToTargetPosition, first.positions)) { case ((currentTime, period), disc) =>
      val currentDiscState = disc.at(currentTime)
      val stepsRequired = currentDiscState.stepsToTargetPosition

      @tailrec
      def loop(stepsTaken: Int): Int = {
        val stepsRequiredToAlignStack = stepsTaken % period
        val stepsRequiredToAlignDisc = disc.at(currentTime + stepsTaken).stepsToTargetPosition
        (stepsRequiredToAlignStack, stepsRequiredToAlignDisc) match {
          case (0, 0) => stepsTaken
          case (n, 0) => loop(stepsTaken + (period - n))
          case (0, m) => loop(stepsTaken + m)
          case (_, _) => throw new IllegalStateException("either n or m should always be 0")
        }
      }

      (currentTime + loop(stepsRequired), period * disc.positions)
    }

    stepsTaken
  }
}

case class Disc(positions: Int, currentPosition: Int, positionInStack: Int) {
  def at(time: Int): Disc = copy(currentPosition = (this.currentPosition + time) % positions)

  def stepsToTargetPosition: Int = {
    val absolutePosition = (positions - (positionInStack % positions)) % positions

    if (absolutePosition < currentPosition) {
      (positions - currentPosition) + absolutePosition
    } else {
      absolutePosition - currentPosition
    }
  }
}

object Disc {
  def parse(description: String): Disc = {
    val extractor = raw"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).".r
    description match {
      case extractor(discNumber, positions, initialPosition) => Disc(positions.toInt, initialPosition.toInt, discNumber.toInt)
      case _ => throw new IllegalArgumentException(s"unparseable disc-ription: $description")
    }
  }
}
