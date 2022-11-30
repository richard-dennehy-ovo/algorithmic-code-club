package aoc2017.day14

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    val exampleGrid = Grid.from("flqrgnkx")
    assert(exampleGrid.used == 8108)
    println(exampleGrid.countRegions)
    val p1Grid = Grid.from("hxtvlmkl")
    println(p1Grid.used)
    println(p1Grid.countRegions)
  }

  case class Cell(on: Boolean, region: Int)
  case class Grid(cells: Vector[Vector[Cell]], nextRegion: Int) {
    def used: Int = cells.map(_.count(_.on)).sum

    override def toString: String = cells.map(row => row.map(c => if (c.on) '#' else '.').mkString).mkString("\n")

    def countRegions: Int = {
      // create set of all on cells
      // start from head
      // find all reachable cells recursively
      // add one to counter once set exhausted
      @tailrec
      def region(toSearch: Set[(Int, Int)], space: Set[(Int, Int)], searched: Set[(Int, Int)]): Set[(Int, Int)] = {
        if (toSearch.isEmpty) {
          searched
        } else {
          val expanded = toSearch.flatMap { case (x, y) =>
            val above = Set(x -> (y - 1))
            val below = Set(x -> (y + 1))
            val left = Set((x - 1) -> y)
            val right = Set((x + 1) -> y)
            above ++ below ++ left ++ right
          }.intersect(space)

          region(expanded, space -- expanded, searched ++ expanded)
        }
      }

      @tailrec
      def regions(searchSpace: Set[(Int, Int)], counter: Int): Int = {
        if (searchSpace.nonEmpty) {
          val (first, rest) = searchSpace.splitAt(1)
          val r = region(first, rest, first)
          regions(searchSpace -- r, counter + 1)
        } else {
          counter
        }
      }

      val on = cells.zipWithIndex.flatMap { case (row, x) =>
        row.zipWithIndex.collect { case (cell, y) if cell.on => x -> y }
      }.toSet

      regions(on, 0)
    }

    def mergeRegions: Grid = {
      // merge adjacent on cells per row
      // merge adjacent rows
      // count distinct region numbers

      cells.zipWithIndex.foldLeft(this) { case (grid, (row, y)) =>
        val updatedGrid = row.zipWithIndex.foldLeft(grid) { case (g, (cell, x)) =>
          if (cell.on) {
            g.copy(cells = g.cells.updated(y, g.cells(y).updated(x, cell.copy(region = g.nextRegion))))
          } else {
            if (x > 0 && row(x - 1).on) {
              g.copy(nextRegion = g.nextRegion + 1)
            } else {
              g
            }
          }
        }

        if (y == 0) {
          updatedGrid
        } else {
          val mergedRow = (0 to 127).foldLeft(updatedGrid.cells(y)) { case (row, x) =>
            val above = updatedGrid.cells(y - 1)(x)
            if (above.on && row(x).on && above.region != row(x).region) {
              row.map(c => c.copy(region = if (c.region == row(x).region) above.region else c.region))
            } else {
              row
            }
          }
          updatedGrid.copy(cells = updatedGrid.cells.updated(y, mergedRow))
        }
      }
    }
  }

  object Grid {
    def from(input: String): Grid = Grid.fromHashes((0 to 127).map(i => s"$input-$i").map(knotHash).toVector)

    def fromHashes(hashes: Vector[Vector[Int]]): Grid = Grid(
      hashes.map { hash =>
        hash.flatMap(paddedBitString(_).toVector.map { bit => Cell(bit == '1', 0) })
      },
      1
    )
  }

  def paddedBitString(value: Int): String = {
    val bitString = value.toBinaryString
    f"$bitString%8s".replace(' ', '0')
  }

  def knotHash(input: String): Vector[Int] = {
    val lengths =
      input.iterator.filterNot(_.isWhitespace).map(_.toInt).toVector ++ Vector(
        17,
        31,
        73,
        47,
        23
      )
    val state =
      Iterator.continually(lengths).take(64).flatten.foldLeft(HashState()) {
        case (state, step) => state.next(step)
      }
    state.denseHash
  }

  case class HashState(values: Vector[Int], position: Int, skip: Int) {
    def next(length: Int): HashState = {
      val newValues = if (position + length >= values.length) {
        // take values from end, append wraparound values to create reversal list
        // reverse list
        // take wraparound from reversed list, append untouched values, append rest of reversed list
        val wraparound = (position + length) % values.length
        val (head, tail) = values.splitAt(position)
        val (wrapped, rest) = head.splitAt(wraparound)
        val reversed = (tail ++ wrapped).reverse
        reversed.takeRight(wraparound) ++ rest ++ reversed.dropRight(wraparound)
      } else {
        val (head, tail) = values.splitAt(position)
        val (section, rest) = tail.splitAt(length)
        head ++ section.reverse ++ rest
      }
      val newPosition = (position + length + skip) % values.length
      val newSkip = skip + 1

      HashState(newValues, newPosition, newSkip)
    }

    def denseHash: Vector[Int] =
      values.grouped(16).map(_.reduce(_ ^ _)).toVector
  }

  object HashState {
    def apply(): HashState = HashState((0 until 256).toVector, 0, 0)
  }
}
