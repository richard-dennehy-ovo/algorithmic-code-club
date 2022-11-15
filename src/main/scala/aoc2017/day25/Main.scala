package aoc2017.day25

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = Source.fromResource("2017/25/example.txt").getLines()
    runP1(example)

    val p1 = Source.fromResource("2017/25/part1.txt").getLines()
    runP1(p1)
  }

  def runP1(rawInput: Iterator[String]): Unit = {
    val input = rawInput.filter(_.nonEmpty).filterNot(_.startsWith("In state"))
    val first = input.next()
    assert(first.endsWith("A."))
    val second = input.next()

    val checksumRegex = raw".*? (\d+) steps.".r
    val steps = second match {
      case checksumRegex(steps) => steps.toInt
      case _ => throw new IllegalStateException(s"Cannot parse checksum instructions from $second")
    }

    val states = Iterator.unfold(input) { remaining =>
      if (remaining.hasNext) {
        Some((State.parse(remaining), remaining))
      } else {
        None
      }
    }.toVector

    val finalTape = (0 until steps).foldLeft(Tape(Vector.fill(8)(false), 3, states, 0)) { case (tape, _) => tape.advance }
    println(finalTape)
    println(finalTape.checksum)
  }

  case class Tape(storage: Vector[Boolean], cursor: Int, states: Vector[State], stateIndex: Int) {
    def checksum: Int = storage.count(identity)
    override def toString: String = {
      val storage = this.storage.zipWithIndex.map { case (cell, idx) =>
        if (idx == cursor) {
          s"[${if (cell) 1 else 0}]"
        } else {
          s" ${if (cell) 1 else 0} "
        }
      }.mkString

      s"...$storage... (next ${('A' + stateIndex).toChar})"
    }
    def advance: Tape = {
      val instruction = if (storage(cursor)) {
        states(stateIndex).if1
      } else {
        states(stateIndex).if0
      }

      val nextStorage = if (instruction.write0) {
        storage.updated(cursor, false)
      } else {
        storage.updated(cursor, true)
      }

      val nextCursor = if (instruction.moveRight) {
        cursor + 1
      } else {
        cursor - 1
      }

      if (nextCursor < 0) {
        Tape(
          Vector.fill(storage.length)(false) ++ nextStorage,
          nextCursor + storage.length,
          states,
          instruction.nextState
        )
      } else if (nextCursor >= storage.length) {
        Tape(
          nextStorage ++ Vector.fill(storage.length)(false),
          nextCursor,
          states,
          instruction.nextState
        )
      } else {
        Tape(
          nextStorage,
          nextCursor,
          states,
          instruction.nextState,
        )
      }
    }
  }

  case class Instruction(write0: Boolean, moveRight: Boolean, nextState: Int) {
    override def toString: String = s"Write ${if (write0) 0 else 1}; Move ${if (moveRight) "Right" else "Left"}; Next: ${('A' + nextState).toChar}"
  }

  object Instruction {
    def parse(text: Iterator[String]): Instruction = {
      val first = text.next()
      assert(first.trim.startsWith("- Write"))
      val write0 = first.endsWith("0.")

      val second = text.next()
      assert(second.trim.startsWith("- Move"))
      val moveRight = second.endsWith("right.")

      val third = text.next()
      assert(third.trim.startsWith("- Continue"))
      val nextState = third.takeRight(2).take(1).head - 'A'

      Instruction(write0, moveRight, nextState)
    }
  }

  case class State(if0: Instruction, if1: Instruction) {
    override def toString: String = s"\n\tIf 0: $if0\n\tIf 1: $if1\n"
  }
  object State {
    def parse(text: Iterator[String]): State = {
      val first = text.next()
      assert(first.endsWith("0:"))
      val if0 = Instruction.parse(text)

      val second = text.next()
      assert(second.endsWith("1:"))
      val if1 = Instruction.parse(text)

      State(if0, if1)
    }
  }
}
