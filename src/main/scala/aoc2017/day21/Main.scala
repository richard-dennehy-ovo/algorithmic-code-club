package aoc2017.day21

import scala.io.Source
import scala.language.postfixOps

object Main {
  def main(args: Array[String]): Unit = {
    val exampleRulesRaw =
      """../.# => ##./#../...
        |.#./..#/### => #..#/..../..../#..#""".stripMargin

    val exampleRules = exampleRulesRaw.linesIterator.map(Rule.parse).toVector

    val expanded = Grid.initial
    Vector(
      "original" -> expanded,
      "rotL" -> expanded.rotateLeft,
      "rotR" -> expanded.rotateRight,
      "flipH" -> expanded.flipH,
      "flipV" -> expanded.flipV,
      "rotR2" -> expanded.rotateRight.rotateRight,
      "rotR flipV" -> expanded.rotateRight.flipV,
      "rotR flipH" -> expanded.rotateRight.flipH,
      "rotL flipV" -> expanded.rotateLeft.flipV,
      "rotL flipH" -> expanded.rotateLeft.flipH,
    ).distinctBy(_._2)
      .foreach(it => println(it._1))

    assert(expand(2, exampleRules).on == 12)

    val actualRules = Source
      .fromResource("2017/21/part1.txt")
      .getLines()
      .map(Rule.parse)
      .toVector
    println(s"part 1: ${expand(5, actualRules).on}")
    println(s"part 2: ${expand(18, actualRules).on}")
  }

  def expand(steps: Int, rules: Vector[Rule]): Grid = {
    (0 until steps).foldLeft(Grid.initial) { (grid, it) =>
      Grid.merge(grid.subdivide.map(_.map { subgrid =>
        val rule = rules
          .find(_.matches(subgrid))
          .getOrElse(
            throw new IllegalStateException(
              s"No pattern matching:\n$subgrid\nafter $it expansions"
            )
          )
        rule.output
      }))
    }
  }

  case class Grid(cells: Vector[Vector[Boolean]]) {
    val on: Int = cells.map(_.count(identity)).sum
    override def toString: String =
      cells.map(_.map(if (_) '#' else '.').mkString).mkString("\n")

    def flipH: Grid = Grid(cells.reverse)
    def flipV: Grid = Grid(cells.map(_.reverse))

    def rotateLeft: Grid = {
      val result = cells.indices.reverse.map { x =>
        cells.indices map { y =>
          cells(y)(x)
        } toVector
      }.toVector

      Grid(result)
    }

    def rotateRight: Grid = {
      val result = cells.indices.map { x =>
        cells.indices.reverse map { y =>
          cells(y)(x)
        } toVector
      }.toVector

      Grid(result)
    }

    def subdivide: Vector[Vector[Grid]] = {
      def inner(size: Int): Vector[Vector[Grid]] = {
        cells.indices
          .grouped(size)
          .map { ys =>
            cells.indices
              .grouped(size)
              .map { xs =>
                Grid(ys.map { y =>
                  xs.map { x =>
                    cells(y)(x)
                  }.toVector
                }.toVector)
              }
              .toVector
          }
          .toVector
      }

      if (cells.head.size % 2 == 0) {
        inner(2)
      } else {
        assert(cells.head.size % 3 == 0)
        inner(3)
      }
    }
  }

  object Grid {
    def parse(text: String): Grid = Grid(
      text
        .split("/")
        .map { row =>
          row.iterator.map {
            case '.' => false
            case '#' => true
            case c =>
              throw new IllegalArgumentException(
                s"Unexpected cell character $c"
              )
          }.toVector
        }
        .toVector
    )

    val initial: Grid = Grid(
      Vector(
        Vector(false, true, false),
        Vector(false, false, true),
        Vector(true, true, true)
      )
    )

    def merge(subgrids: Vector[Vector[Grid]]): Grid = {
      val result = subgrids.flatMap { gridRow =>
        gridRow.head.cells.indices.map { y =>
          gridRow.flatMap { grid =>
            grid.cells.indices.map { x =>
              grid.cells(y)(x)
            }
          }
        }
      }
      Grid(result)
    }
  }

  case class Rule(input: Grid, output: Grid) {
    def matches(grid: Grid): Boolean = {
      grid.on == input.on && Iterator(
        grid,
        grid.rotateLeft,
        grid.rotateRight,
        grid.flipH,
        grid.flipV,
        grid.rotateRight.rotateRight,
        grid.rotateRight.flipV,
        grid.rotateRight.flipH,
      ).contains(input)
    }

    override def toString: String = s"$input\n ↓\n$output\n"
  }
  object Rule {
    def parse(text: String): Rule = {
      val pair = text.split(" => ")
      assert(pair.length == 2)
      Rule(Grid.parse(pair(0)), Grid.parse(pair(1)))
    }
  }
}
