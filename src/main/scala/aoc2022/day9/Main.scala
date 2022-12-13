package aoc2022.day9

import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """R 4
                    |U 4
                    |L 3
                    |D 1
                    |R 4
                    |D 1
                    |L 5
                    |R 2""".stripMargin
    val exampleFinalState =
      example.linesIterator.map(Direction.parse).flatMap(_.asSingleSteps).foldLeft(State.initial(1)) {
        case (state, direction) =>
          state.next(direction)
      }
    println(exampleFinalState.visited.size)
    exampleFinalState.visited.toVector.sorted.foreach(println)

    val inputDirections = Source
      .fromResource("2022/9/p1.txt")
      .getLines()
      .map(Direction.parse)
      .flatMap(_.asSingleSteps)
      .toVector
    val p1 = inputDirections.foldLeft(State.initial(1))((state, direction) =>
      state.next(direction)
    )
    println(p1.visited.size)

    val p2Example = """R 5
                      |U 8
                      |L 8
                      |D 3
                      |R 17
                      |D 10
                      |L 25
                      |U 20""".stripMargin
    val p2ExampleFinalState = p2Example.linesIterator
      .map(Direction.parse)
      .foldLeft(State.initial(9))((state, direction) => state.next(direction))
    println(p2ExampleFinalState.visited.size)
    p2ExampleFinalState.visited.toVector.sorted.foreach(println)

    val p2 = inputDirections.foldLeft(State.initial(9))((state, direction) =>
      state.next(direction)
    )
    println(p2.visited.size)
  }

  case class State(
      knots: Vector[Position],
      tail: Position,
      visited: Set[Position]
  ) {
    def next(direction: Direction): State = {
      val newHead = knots.head.move(direction)
      val newKnots = knots.tail.foldLeft(Vector(newHead)) {
        case (previousKnots, knot) =>
          val previous = previousKnots.last
          previousKnots :+ knot.moveTowards(previous).last
      }
      val movedTail = tail.moveTowards(newKnots.last)
      State(newKnots, movedTail.last, visited ++ movedTail)
    }
  }
  object State {
    def initial(knots: Int): State =
      State(Vector.fill(knots)(Position(0, 0)), Position(0, 0), Set.empty)
  }

  case class Position(x: Int, y: Int) {
    def move(direction: Direction): Position = direction match {
      case Left(steps)  => Position(x - steps, y)
      case Up(steps)    => Position(x, y + steps)
      case Right(steps) => Position(x + steps, y)
      case Down(steps)  => Position(x, y - steps)
    }

    def moveTowards(target: Position): Vector[Position] = {
      @tailrec def loop(
          current: Position,
          visited: Vector[Position]
      ): Vector[Position] = {
        val xDelta = target.x - current.x
        val yDelta = target.y - current.y
        if (xDelta.abs <= 1 && yDelta.abs <= 1) {
          visited :+ current
        } else {
          loop(
            Position(current.x + xDelta.sign, current.y + yDelta.sign),
            visited :+ current
          )
        }
      }

      loop(this, Vector.empty)
    }
  }
  object Position {
    implicit val ordering: Ordering[Position] = new Ordering[Position] {
      override def compare(first: Position, second: Position): Int = {
        if (first.x == second.x) {
          first.y.compareTo(second.y)
        } else {
          first.x.compareTo(second.x)
        }
      }
    }
  }

  sealed trait Direction {
    def asSingleSteps: Vector[Direction] = this match {
      case Left(steps)  => (0 until steps).map(_ => Left(1)).toVector
      case Up(steps)    => (0 until steps).map(_ => Up(1)).toVector
      case Right(steps) => (0 until steps).map(_ => Right(1)).toVector
      case Down(steps)  => (0 until steps).map(_ => Down(1)).toVector
    }
  }
  case class Left(steps: Int) extends Direction
  case class Up(steps: Int) extends Direction
  case class Right(steps: Int) extends Direction
  case class Down(steps: Int) extends Direction

  object Direction {
    def parse(line: String): Direction = {
      val regex = raw"(.) (\d*)".r
      line match {
        case regex("L", steps) => Left(steps.toInt)
        case regex("U", steps) => Up(steps.toInt)
        case regex("R", steps) => Right(steps.toInt)
        case regex("D", steps) => Down(steps.toInt)
        case _                 => throw new IllegalArgumentException(s"Unparseable: $line")
      }
    }
  }
}
