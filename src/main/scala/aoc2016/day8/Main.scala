package aoc2016.day8

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    println(s"Example: ${exampleLitPixels()}")

    val screen = challenge()
    println(s"part 1: ${screen.litPixels}")
    println(s"part 2: ${screen.printDebugString()}")
  }

  private def exampleLitPixels(): Int = {
    val input = Seq(
      "rect 3x2",
      "rotate column x=1 by 1",
      "rotate row y=0 by 4"
    )

    val commands = input.map(Command.parse)
    val finalScreen = commands.foldLeft(Screen.empty(7, 3))(_.applyCommand(_))
    finalScreen.litPixels
  }

  private def challenge(): Screen = {
    val input =
      Source
        .fromResource("2016/8/part 1.txt")
        .getLines()
        .map(Command.parse)

    input.foldLeft(Screen.empty(50, 6))(_.applyCommand(_))
  }
}

sealed trait Command
object Command {
  case class Rect(width: Int, height: Int) extends Command
  case class RotateColumn(index: Int, by: Int) extends Command
  case class RotateRow(index: Int, by: Int) extends Command

  def parse(input: String): Command = {
    val rectRegex = raw"rect (\d+)x(\d+)".r
    val rotateColumnRegex = raw"rotate column x=(\d+) by (\d+)".r
    val rotateRowRegex = raw"rotate row y=(\d+) by (\d+)".r

    input match {
      case rectRegex(width, height)     => Rect(width.toInt, height.toInt)
      case rotateColumnRegex(index, by) => RotateColumn(index.toInt, by.toInt)
      case rotateRowRegex(index, by)    => RotateRow(index.toInt, by.toInt)
      case _                            => throw new IllegalArgumentException(s"unknown command $input")
    }
  }
}

case class Screen(cells: Vector[Row]) {
  private val width = cells.head.length
  private val height = cells.length

  def printDebugString(): Screen = {
    cells.foreach(row => println(row.map(if (_) '#' else '.').mkString))
    println()
    this
  }

  def applyCommand(command: Command): Screen = {
    command match {
      case Command.Rect(width, height)     => rect(width, height)
      case Command.RotateColumn(index, by) => rotateColumn(index, by)
      case Command.RotateRow(index, by)    => rotateRow(index, by)
    }
  }

  def rect(width: Int, height: Int): Screen = {
    Screen(cells.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        (x < width && y < height) || cell
      }
    })
  }

  def rotateRow(index: Int, by: Int): Screen = {
    Screen(cells.zipWithIndex.map {
      case (row, `index`) =>
        row.indices.map(idx => row((idx - by + width) % width)).toVector
      case (row, _) => row
    })
  }

  def rotateColumn(index: Int, by: Int): Screen = {
    Screen(cells.zipWithIndex.map { case (row, rowIndex) =>
      row.zipWithIndex.map {
        case (_, `index`) => cells((rowIndex - by + height) % height)(index)
        case (cell, _)    => cell
      }
    })
  }

  def litPixels: Int =
    cells.foldLeft(0)((acc, row) => acc + row.count(identity))
}

object Screen {
  def empty(width: Int, height: Int): Screen = Screen(
    Vector.fill(height)(Vector.fill(width)(false))
  )
}
