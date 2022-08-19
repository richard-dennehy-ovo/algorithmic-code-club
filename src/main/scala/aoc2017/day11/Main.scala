package aoc2017.day11

import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val examples = Map(
      "ne,ne,ne" -> 3,
      "ne,ne,sw,sw" -> 0,
      "ne,ne,s,s" -> 2,
      "se,sw,se,sw,sw" -> 3
    )

    examples.foreach { case (input, expected) =>
      val steps = minSteps(input)
      assert(expected == steps)
    }

    val part1 = Source.fromResource("2017/11/part1.txt").mkString
    println(s"part1: ${minSteps(part1)}")
  }

  private val simplifications = Seq(
    Rule(Direction.NorthWest, Direction.SouthEast, Adjacency(Direction.NorthEast, Direction.North), Adjacency(Direction.South, Direction.SouthWest)),
    Rule(Direction.North, Direction.South, Adjacency(Direction.SouthWest, Direction.NorthWest), Adjacency(Direction.SouthEast, Direction.NorthEast)),
    Rule(Direction.NorthEast, Direction.SouthWest, Adjacency(Direction.NorthWest, Direction.North), Adjacency(Direction.South, Direction.SouthEast)),
    Rule(Direction.SouthEast, Direction.NorthWest, Adjacency(Direction.North, Direction.NorthEast), Adjacency(Direction.SouthWest, Direction.South)),
    Rule(Direction.South, Direction.North, Adjacency(Direction.NorthEast, Direction.SouthEast), Adjacency(Direction.NorthEast, Direction.SouthEast)),
    Rule(Direction.SouthWest, Direction.NorthEast, Adjacency(Direction.SouthEast, Direction.South), Adjacency(Direction.North, Direction.NorthWest)),
  )

  private def minSteps(input: String) = {
    @tailrec def simplify(state: Map[Direction, Int]): Map[Direction, Int] = {
      val updated = simplifications.foldLeft(state) { case (state, rule) => rule.applyTo(state) }
      if (updated != state) {
        simplify(updated)
      } else {
        state
      }
    }

    val initial: Map[Direction, Int] = Map(
      Direction.NorthWest -> 0,
      Direction.NorthEast -> 0,
      Direction.North -> 0,
      Direction.SouthEast -> 0,
      Direction.South -> 0,
      Direction.SouthWest -> 0,
    )
    val state = input.split(",").map(Direction.parse).foldLeft(initial)(_.updatedWith(_)(_.map(_ + 1)))

    val simplified = simplify(state)
    simplified.values.foldLeft(0) { case (sum, steps) =>
      assert(steps >= 0)
      sum + steps
    }
  }

  case class Rule(matchDirection: Direction, inverse: Direction, adjacentLeft: Adjacency, adjacentRight: Adjacency) {
    def applyTo(state: Map[Direction, Int]): Map[Direction, Int] = if (state(matchDirection) > 0) {
      if (state(inverse) > 0) {
        val eliminated = math.min(state(matchDirection), state(inverse))
        state
          .updatedWith(matchDirection)(_.map(_ - eliminated))
          .updatedWith(inverse)(_.map(_ - eliminated))
      } else if (state(adjacentLeft.matchDirection) > 0) {
        val merged = math.min(state(matchDirection), state(adjacentLeft.matchDirection))
        state
          .updatedWith(matchDirection)(_.map(_ - merged))
          .updatedWith(adjacentLeft.matchDirection)(_.map(_ - merged))
          .updatedWith(adjacentLeft.replace)(_.map(_ + merged))
      } else if (state(adjacentRight.matchDirection) > 0) {
        val merged = math.min(state(matchDirection), state(adjacentRight.matchDirection))
        state
          .updatedWith(matchDirection)(_.map(_ - merged))
          .updatedWith(adjacentRight.matchDirection)(_.map(_ - merged))
          .updatedWith(adjacentRight.replace)(_.map(_ + merged))
      } else {
        state
      }
    } else {
      state
    }
  }
  case class Adjacency(matchDirection: Direction, replace: Direction)

  sealed trait Direction
  object Direction {
    case object NorthWest extends Direction
    case object North extends Direction
    case object NorthEast extends Direction
    case object SouthWest extends Direction
    case object South extends Direction
    case object SouthEast extends Direction

    def parse(input: String): Direction = input.trim match {
      case "nw" => NorthWest
      case "n"  => North
      case "ne" => NorthEast
      case "se" => SouthEast
      case "s"  => South
      case "sw" => SouthWest
      case _ =>
        throw new IllegalArgumentException(s"Unexpected direction $input")
    }
  }
}
