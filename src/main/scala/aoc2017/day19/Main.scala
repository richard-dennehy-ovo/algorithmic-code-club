package aoc2017.day19

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    assert(solve("example.txt") == ("ABCDEF", 38))
    println(s"part1: ${solve("part1.txt")}")
  }

  def solve(file: String): (String, Int) = {
    val cells = Source
      .fromResource(s"2017/19/$file")
      .getLines()
      .map(_.map(Cell.parse).toVector)
      .toVector

    val start = cells.head.indexOf(Some(Vertical))
    val diagram = Diagram(cells)
    val initial = State(start, 0, Down, "")
    val steps = Iterator
      .iterate(Option(initial))(_.flatMap(_.next(diagram)))
      .takeWhile(_.isDefined)
      .toVector
    (steps.last.get.seen, steps.length)
  }

  case class Diagram(cells: Vector[Vector[Option[Cell]]]) {
    def get(x: Int, y: Int): Option[Cell] =
      cells.lift(y).flatMap(_.lift(x)).flatten
  }

  case class State(
      x: Int,
      y: Int,
      direction: Direction,
      seen: String
  ) {
    def next(diagram: Diagram): Option[State] = {
//      println(s"(${this.x}, ${this.y}): ${diagram.get(this.x, this.y)}")
      val (x, y) = direction match {
        case Up    => (this.x, this.y - 1)
        case Down  => (this.x, this.y + 1)
        case Left  => (this.x - 1, this.y)
        case Right => (this.x + 1, this.y)
      }

      diagram.get(x, y).map {
        case Horizontal | Vertical => copy(x = x, y = y)
        // assuming direction should always change
        case Cross =>
          if (direction == Up || direction == Down) {
            if (diagram.get(x - 1, y).isDefined) {
              copy(x = x, y = y, direction = Left)
            } else {
              copy(x = x, y = y, direction = Right)
            }
          } else {
            if (diagram.get(x, y - 1).isDefined) {
              copy(x = x, y = y, direction = Up)
            } else {
              copy(x = x, y = y, direction = Down)
            }
          }
        case Letter(value) => copy(x = x, y = y, seen = seen + value)
      }
    }
  }

  sealed trait Direction
  object Up extends Direction
  object Down extends Direction
  object Left extends Direction
  object Right extends Direction

  sealed trait Cell {
    override def toString: String = this match {
      case Horizontal    => "-"
      case Vertical      => "|"
      case Cross         => "+"
      case Letter(value) => value.toString
    }
  }
  case object Horizontal extends Cell
  case object Vertical extends Cell
  case object Cross extends Cell
  case class Letter(value: Char) extends Cell

  object Cell {
    def parse(value: Char): Option[Cell] = value match {
      case ' ' => None
      case '-' => Some(Horizontal)
      case '|' => Some(Vertical)
      case '+' => Some(Cross)
      case _   => Some(Letter(value))
    }
  }
}
