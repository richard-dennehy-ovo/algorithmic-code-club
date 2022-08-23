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
      val result = input.split(",").map(Directions.parse)
        .reduce(_ + _)
      val steps = minSteps(result)
      assert(steps == expected, s"$steps != $expected")
    }

    val part1Input = Source.fromResource("2017/11/part1.txt").mkString
    val (p1, p2) = part1Input.split(",").map(Directions.parse).foldLeft(Vec2(0.0, 0.0) -> 0) { case ((location, maxDistance), direction) =>
      val newLocation = location + direction
      val distance = minSteps(newLocation)
      (newLocation, math.max(maxDistance, distance))
    }
    println(s"part1: ${minSteps(p1)}")
    println(s"part2: $p2")
  }

  def minSteps(vec2: Vec2): Int = {
    @tailrec def acc(steps: Int, remaining: Vec2): Int = {
      val epsilon = Math.ulp(1.0.toFloat)
      if (remaining.x.abs <= epsilon && remaining.y.abs <= epsilon) {
        steps
      } else {
        val dir = remaining match {
          case Vec2(x, y) if x > epsilon && y > epsilon => Directions.NorthEast
          case Vec2(x, y) if x < -epsilon && y > epsilon => Directions.NorthWest
          case Vec2(_, y) if y > epsilon => Directions.North
          case Vec2(x, _) if x > epsilon => Directions.SouthEast
          case Vec2(x, _) if x < -epsilon => Directions.SouthWest
          case Vec2(_, _) => Directions.South
        }
        acc(steps + 1, remaining - dir)
      }
    }

    acc(0, vec2)
  }

  case class Vec2(x: Double, y: Double) {
    def +(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
    def -(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
  }

  object Vec2 {
    def fromDegrees(degrees: Double): Vec2 = Vec2(math.cos(degrees.toRadians), math.sin(degrees.toRadians))
  }

  object Directions {
    val NorthEast = Vec2.fromDegrees(30)
    val North = Vec2(0.0, 1.0)
    val NorthWest = Vec2.fromDegrees(150)
    val SouthWest = Vec2.fromDegrees(210)
    val South = Vec2(0.0, -1.0)
    val SouthEast = Vec2.fromDegrees(330)

    def parse(input: String): Vec2 = input.trim match {
      case "ne" => NorthEast
      case "n" => North
      case "nw" => NorthWest
      case "sw" => SouthWest
      case "s" => South
      case "se" => SouthEast
      case _ => throw new IllegalArgumentException(s"Unexpected direction $input")
    }
  }
}
