package aoc2017.day22

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """..#
                    |#..
                    |...""".stripMargin
    val initial = P1State.parse(example.linesIterator)
    val state = (0 until 70).foldLeft(initial) { case (state, _) =>
      state.advance
    }
    println(state)
    println(state.infected)

    val part1 = P1State.parse(Source.fromResource("2017/22/part1.txt").getLines())
    val finalState = (0 until 10000).foldLeft(part1) { case (state, _) =>
      state.advance
    }
    println(finalState.infected)

    val p2ExInitial = P2State.fromP1State(initial)
    val p2ExState100 = (0 until 100).foldLeft(p2ExInitial) { case (state, _) => state.advance }
    println(p2ExState100.infected)
    val p2ExState10000000 = (0 until 10000000).foldLeft(p2ExInitial) { case (state, _) => state.advance }
    println(p2ExState10000000.infected)

    val part2 = P2State.fromP1State(part1)
    val finalP2State = (0 until 10000000).foldLeft(part2) { case (state, _) => state.advance }
    println(finalP2State.infected)
  }

  case class P1State(map: Vector[Vector[Boolean]], virus: Virus, infected: Int) {
    def advance: P1State = {
      val alreadyInfected = map(virus.y)(virus.x)
      val newVirus = if (alreadyInfected) {
        virus.turnRight.advance
      } else {
        virus.turnLeft.advance
      }
      val newMap =
        map.updated(virus.y, map(virus.y).updated(virus.x, !alreadyInfected))
      val newState =
        P1State(newMap, newVirus, infected + (if (alreadyInfected) 0 else 1))

      if (
        newVirus.x < 0 || newVirus.y < 0 || newVirus.x >= map.length || newVirus.y >= map.length
      ) {
        newState.expand
      } else {
        newState
      }
    }

    def expand: P1State = {
      val expandSize = (map.length - 1) / 2
      val newMap =
        Vector.fill(expandSize)(
          Vector.fill(map.length + expandSize * 2)(false)
        ) ++
          map.map(row =>
            Vector.fill(expandSize)(false) ++ row ++ Vector.fill(expandSize)(
              false
            )
          ) ++
          Vector.fill(expandSize)(
            Vector.fill(map.length + expandSize * 2)(false)
          )
      assert(newMap.size % 2 == 1)
      assert(newMap.head.size == newMap.size)

      P1State(
        newMap,
        virus.copy(x = virus.x + expandSize, y = virus.y + expandSize),
        infected
      )
    }

    override def toString: String = map.zipWithIndex
      .map { case (rows, y) =>
        rows.zipWithIndex.map { case (cell, x) =>
          if (virus.x == x && virus.y == y) {
            if (cell) {
              s"${virus.direction}#${virus.direction}"
            } else {
              s"${virus.direction}.${virus.direction}"
            }
          } else if (cell) {
            " # "
          } else {
            " . "
          }
        }.mkString
      }
      .mkString("\n")
  }

  case class P2State(map: Vector[Vector[CellState]], virus: Virus, infected: Int) {
    def advance: P2State = {
      val currentCellState = map(virus.y)(virus.x)
      val newVirus = currentCellState match {
        case CellState.Clean => virus.turnLeft.advance
        case CellState.Weakened => virus.advance
        case CellState.Flagged => virus.reverse.advance
        case CellState.Infected => virus.turnRight.advance
      }
      val newCellState = currentCellState match {
        case CellState.Clean => CellState.Weakened
        case CellState.Weakened => CellState.Infected
        case CellState.Flagged => CellState.Clean
        case CellState.Infected => CellState.Flagged
      }
      val newMap =
        map.updated(virus.y, map(virus.y).updated(virus.x, newCellState))
      val newState =
        P2State(newMap, newVirus, infected + (if (newCellState == CellState.Infected) 1 else 0))

      if (
        newVirus.x < 0 || newVirus.y < 0 || newVirus.x >= map.length || newVirus.y >= map.length
      ) {
        newState.expand
      } else {
        newState
      }
    }

    def expand: P2State = {
      val expandSize = (map.length - 1) / 2
      val newMap =
        Vector.fill(expandSize)(
          Vector.fill(map.length + expandSize * 2)(CellState.Clean)
        ) ++
          map.map(row =>
            Vector.fill(expandSize)(CellState.Clean) ++ row ++ Vector.fill(expandSize)(
              CellState.Clean
            )
          ) ++
          Vector.fill(expandSize)(
            Vector.fill(map.length + expandSize * 2)(CellState.Clean)
          )
      assert(newMap.size % 2 == 1)
      assert(newMap.head.size == newMap.size)

      P2State(
        newMap,
        virus.copy(x = virus.x + expandSize, y = virus.y + expandSize),
        infected
      )
    }

    override def toString: String = map.zipWithIndex
      .map { case (rows, y) =>
        rows.zipWithIndex.map { case (cell, x) =>
          if (virus.x == x && virus.y == y) {
              s"${virus.direction}$cell${virus.direction}"
          } else {
            s" $cell "
          }
        }.mkString
      }
      .mkString("\n")
  }

  case class Virus(x: Int, y: Int, direction: Direction) {
    def turnLeft: Virus = copy(direction = direction match {
      case Direction.Up    => Direction.Left
      case Direction.Left  => Direction.Down
      case Direction.Down  => Direction.Right
      case Direction.Right => Direction.Up
    })

    def turnRight: Virus = copy(direction = direction match {
      case Direction.Up    => Direction.Right
      case Direction.Left  => Direction.Up
      case Direction.Down  => Direction.Left
      case Direction.Right => Direction.Down
    })

    def reverse: Virus = copy(direction = direction match {
      case Direction.Up => Direction.Down
      case Direction.Left => Direction.Right
      case Direction.Down => Direction.Up
      case Direction.Right => Direction.Left
    })

    def advance: Virus = direction match {
      case Direction.Up    => Virus(x, y - 1, direction)
      case Direction.Left  => Virus(x - 1, y, direction)
      case Direction.Down  => Virus(x, y + 1, direction)
      case Direction.Right => Virus(x + 1, y, direction)
    }
  }

  sealed trait Direction {
    override def toString: String = {
      this match {
        case Direction.Up    => "↑"
        case Direction.Down  => "↓"
        case Direction.Left  => "←"
        case Direction.Right => "→"
      }
    }
  }
  object Direction {
    object Up extends Direction
    object Left extends Direction
    object Down extends Direction
    object Right extends Direction
  }

  sealed trait CellState {
    override def toString: String = this match {
      case CellState.Clean => "."
      case CellState.Weakened => "W"
      case CellState.Flagged => "F"
      case CellState.Infected => "#"
    }
  }

  object CellState {
    object Clean extends CellState
    object Weakened extends CellState
    object Flagged extends CellState
    object Infected extends CellState
  }

  object P1State {
    def parse(lines: Iterator[String]): P1State = {
      val cells = lines.map { line =>
        line.map(_ == '#').toVector
      }.toVector
      assert(cells.length % 2 == 1)
      assert(cells.length == cells.head.length)
      P1State(cells, Virus(cells.length / 2, cells.length / 2, Direction.Up), 0)
    }
  }

  object P2State {
    def fromP1State(p1State: P1State): P2State = {
      P2State(p1State.map.map(_.map(if (_) CellState.Infected else CellState.Clean)), p1State.virus, p1State.infected)
    }
  }
}
