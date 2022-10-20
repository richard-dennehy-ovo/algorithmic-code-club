package aoc2017.day22

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """..#
                    |#..
                    |...""".stripMargin
    val initial = State.parse(example.linesIterator)
    val state = (0 until 70).foldLeft(initial) { case (state, _) =>
      state.advance
    }
    println(state)
    println(state.infected)

    val part1 = State.parse(Source.fromResource("2017/22/part1.txt").getLines())
    val finalState = (0 until 10000).foldLeft(part1) { case (state, _) =>
      state.advance
    }
    println(finalState.infected)
  }

  case class State(map: Vector[Vector[Boolean]], virus: Virus, infected: Int) {
    def advance: State = {
      val alreadyInfected = map(virus.y)(virus.x)
      val newVirus = if (alreadyInfected) {
        virus.advanceRight
      } else {
        virus.advanceLeft
      }
      val newMap =
        map.updated(virus.y, map(virus.y).updated(virus.x, !alreadyInfected))
      val newState =
        State(newMap, newVirus, infected + (if (alreadyInfected) 0 else 1))

      if (
        newVirus.x < 0 || newVirus.y < 0 || newVirus.x >= map.length || newVirus.y >= map.length
      ) {
        newState.expand
      } else {
        newState
      }
    }

    def expand: State = {
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

      State(
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
              s"{${virus.direction}}"
            } else {
              s"[${virus.direction}]"
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

  case class Virus(x: Int, y: Int, direction: Direction) {
    def advanceLeft: Virus = advance(direction match {
      case Direction.Up    => Direction.Left
      case Direction.Left  => Direction.Down
      case Direction.Down  => Direction.Right
      case Direction.Right => Direction.Up
    })

    def advanceRight: Virus = advance(direction match {
      case Direction.Up    => Direction.Right
      case Direction.Left  => Direction.Up
      case Direction.Down  => Direction.Left
      case Direction.Right => Direction.Down
    })

    private def advance(newDirection: Direction): Virus = newDirection match {
      case Direction.Up    => Virus(x, y - 1, newDirection)
      case Direction.Left  => Virus(x - 1, y, newDirection)
      case Direction.Down  => Virus(x, y + 1, newDirection)
      case Direction.Right => Virus(x + 1, y, newDirection)
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

  object State {
    def parse(lines: Iterator[String]): State = {
      val cells = lines.map { line =>
        line.map(_ == '#').toVector
      }.toVector
      assert(cells.length % 2 == 1)
      assert(cells.length == cells.head.length)
      State(cells, Virus(cells.length / 2, cells.length / 2, Direction.Up), 0)
    }
  }
}
