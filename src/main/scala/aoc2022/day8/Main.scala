package aoc2022.day8

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """30373
                    |25512
                    |65332
                    |33549
                    |35390""".stripMargin
    val exampleGrid = example.linesIterator.map(_.toVector.map(_.asDigit)).toVector
    println(countVisible(exampleGrid))

    val actualGrid = Source.fromResource("2022/8/p1.txt").getLines().map(_.toVector.map(_.asDigit)).toVector
    println(countVisible(actualGrid))

    println(mostScenic(exampleGrid))
    println(mostScenic(actualGrid))
  }

  private def countVisible(grid: Vector[Vector[Int]]): Int = {
    grid.zipWithIndex.map { case (row, y) =>
      if (y == 0 || y == grid.length - 1) {
        grid.length
      } else {
        row.zipWithIndex.map { case (height, x) =>
          if (x == 0 || x == row.length - 1) {
            1
          } else {
            val hiddenLeft = (0 until x).findLast(row(_) >= height)
            val hiddenRight = (x + 1 until row.length).find(row(_) >= height)
            val hiddenUp = (0 until y).findLast(grid(_)(x) >= height)
            val hiddenDown = (y + 1 until grid.length).find(grid(_)(x) >= height)

            if (hiddenLeft.isDefined && hiddenRight.isDefined && hiddenUp.isDefined && hiddenDown.isDefined) 0 else 1
          }
        }.sum
      }
    }.sum
  }

  private def mostScenic(grid: Vector[Vector[Int]]): Int = {
    grid.zipWithIndex.map { case (row, y) =>
      if (y == 0 || y == grid.length - 1) {
        0
      } else {
        row.zipWithIndex.map { case (height, x) =>
          if (x == 0 || x == row.length - 1) {
            0
          } else {
            val visibleLeft = math.min((0 until x).reverse.takeWhile(row(_) < height).length + 1, x)
            val visibleRight = math.min((x + 1 until row.length).takeWhile(row(_) < height).length + 1, row.length - 1 - x)
            val visibleUp = math.min((0 until y).reverse.takeWhile(grid(_)(x) < height).length + 1, y)
            val visibleDown = math.min((y + 1 until grid.length).takeWhile(grid(_)(x) < height).length + 1, grid.length - 1 - y)

            visibleLeft * visibleRight * visibleUp * visibleDown
          }
        }.max
      }
    }.max
  }
}
