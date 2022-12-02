package aoc2022.day2

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """A Y
                    |B X
                    |C Z""".stripMargin

    assert(score(example.linesIterator.map(parseRoundP1)) == 15)

    val p1Input = Source.fromResource("2022/2/p1.txt").getLines().toVector
    assert(score(p1Input.iterator.map(parseRoundP1)) == 9651)

    assert(score(example.linesIterator.map(parseRoundP2)) == 12)
    println(score(p1Input.iterator.map(parseRoundP2)))
  }

  def score(input: Iterator[(Move, Move)]): Int = {
    input.map { case (o, r) =>
      val outcome = if (o.winsAgainst == r) {
        0
      } else if (o.losesAgainst == r) {
        6
      } else {
        3
      }

      outcome + r.value
    }.sum
  }

  def parseRoundP1(line: String): (Move, Move) = {
    val opponent = line.head match {
      case 'A' => Move.Rock
      case 'B' => Move.Paper
      case 'C' => Move.Scissors
      case other => throw new IllegalStateException(s"Unexpected move $other")
    }
    val response = line(2) match {
      case 'X' => Move.Rock
      case 'Y' => Move.Paper
      case 'Z' => Move.Scissors
      case other => throw new IllegalStateException(s"Unexpected move $other")
    }
    opponent -> response
  }

  def parseRoundP2(line: String): (Move, Move) = {
    val opponent = line.head match {
      case 'A' => Move.Rock
      case 'B' => Move.Paper
      case 'C' => Move.Scissors
      case other => throw new IllegalStateException(s"Unexpected move $other")
    }
    val response = line(2) match {
      case 'X' => opponent.winsAgainst
      case 'Y' => opponent
      case 'Z' => opponent.losesAgainst
      case other => throw new IllegalStateException(s"Unexpected move $other")
    }
    opponent -> response
  }

  sealed trait Move {
    val value: Int
    val winsAgainst: Move
    val losesAgainst: Move
  }
  object Move {
    case object Rock extends Move {
      override val value: Int = 1
      override val winsAgainst: Move = Move.Scissors
      override val losesAgainst: Move = Move.Paper
    }
    case object Paper extends Move {
      override val value: Int = 2
      override val winsAgainst: Move = Move.Rock
      override val losesAgainst: Move = Move.Scissors
    }
    case object Scissors extends Move {
      override val value: Int = 3
      override val winsAgainst: Move = Move.Paper
      override val losesAgainst: Move = Move.Rock
    }
  }
}
