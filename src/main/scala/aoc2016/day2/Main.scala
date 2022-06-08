package aoc2016.day2

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val basicKeypad = Keypad(Vector(
      Vector(Some('1'), Some('2'), Some('3')),
      Vector(Some('4'), Some('5'), Some('6')),
      Vector(Some('7'), Some('8'), Some('9')),
    ))
    val basicState = State(1, 1, basicKeypad)

    val complicatedKeypad = Keypad(Vector(
      Vector(None, None, Some('1'), None, None),
      Vector(None, Some('2'), Some('3'), Some('4'), None),
      Vector(Some('5'), Some('6'), Some('7'), Some('8'), Some('9')),
      Vector(None, Some('A'), Some('B'), Some('C'), None),
      Vector(None, None, Some('D'), None, None),
    ))
    val complicatedState = State(0, 2, complicatedKeypad)

    println(s"example: ${code("example", basicState)}")
    println(s"part 1: ${code("part 1", basicState)}")

    println(s"part 2 example: ${code("example", complicatedState)}")
    println(s"part 2: ${code("part 1", complicatedState)}")
  }

  private def code(fileName: String, initialState: State) = {
    val (_, code) = Source
      .fromResource(s"2016/2/$fileName.txt")
      .getLines()
      .foldLeft((initialState, "")) { case ((state, keys), instructions) =>
        val newState =
          instructions.map(Instruction.parse).foldLeft(state)(_.updated(_))
        (newState, keys + newState.key)
      }

    code
  }
}

case class State(xPosition: Int, yPosition: Int, keypad: Keypad) {
  def key: Char = maybeKey.getOrElse(throw new IllegalStateException(s"No key for key position ($xPosition, $yPosition)"))
  private def maybeKey: Option[Char] = {
    if (yPosition < 0 || yPosition >= keypad.keys.length || xPosition < 0 || xPosition >= keypad.keys(yPosition).length) {
      None
    } else {
      keypad.keys(yPosition)(xPosition)
    }
  }

  def updated(instruction: Instruction): State = {
    def maybeUpdated = instruction match {
      case Instruction.Up => copy(yPosition = yPosition - 1)
      case Instruction.Down => copy(yPosition = yPosition + 1)
      case Instruction.Left => copy(xPosition = xPosition - 1)
      case Instruction.Right => copy(xPosition = xPosition + 1)
    }

    if (maybeUpdated.maybeKey.isEmpty) {
      this
    } else {
      maybeUpdated
    }
  }
}

case class Keypad(keys: Vector[Vector[Option[Char]]])

sealed trait Instruction
object Instruction {
  case object Up extends Instruction
  case object Down extends Instruction
  case object Left extends Instruction
  case object Right extends Instruction

  def parse(char: Char): Instruction = {
    char match {
      case 'U' => Instruction.Up
      case 'D' => Instruction.Down
      case 'L' => Instruction.Left
      case 'R' => Instruction.Right
      case _ =>
        throw new IllegalArgumentException(s"unknown instruction '$char'")
    }
  }
}
