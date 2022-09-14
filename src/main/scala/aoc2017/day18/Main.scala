package aoc2017.day18

import aoc2017.day18.Main.Instruction.RegOrValue

import java.time.Instant
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = Source
      .fromResource("2017/18/example.txt")
      .getLines()
      .map(Instruction.parse)
      .toVector
    println(p1(example))

    val part1 = Source
      .fromResource("2017/18/part1.txt")
      .getLines()
      .map(Instruction.parse)
      .toVector
    println(p1(part1))

    val p2Example = Source
      .fromResource("2017/18/example2.txt")
      .getLines()
      .map(Instruction.parse)
      .toVector
    println(p2(p2Example))
    println(p2(part1))
  }

  private def p1(instructions: Vector[Instruction]): Option[Long] = {
    Iterator
      .unfold(P1State(0, None, Map.empty.withDefaultValue(0)))(
        _.next(instructions)
      )
      .toVector
      .last
  }

  private def p2(instructions: Vector[Instruction]): Int = {
    Iterator
      .unfold(
        (
          P2State(Map('p' -> 0L).withDefaultValue(0L), 0, None, Vector.empty),
          P2State(Map('p' -> 1L).withDefaultValue(0L), 0, None, Vector.empty),
          0,
          true
        )
      ) { case (fst, snd, sendCount, processFst) =>
        if (processFst) {
          fst.next(instructions) match {
            case Some(next) =>
              Some((sendCount, (next.updateQueues(snd.sent), snd.updateQueues(next.sent), sendCount, true)))
            case None =>
              println("processing second")
              snd.next(instructions).map { next =>
                val nextSendCount = sendCount + (if (next.sent.nonEmpty) 1 else 0)
                (nextSendCount, (fst.updateQueues(next.sent), next.updateQueues(fst.sent), nextSendCount, false))
              }
          }
        } else {
          snd.next(instructions) match {
            case Some(next) =>
              val nextSendCount = sendCount + (if (next.sent.nonEmpty) 1 else 0)
              Some((nextSendCount, (fst.updateQueues(next.sent), next.updateQueues(fst.sent), nextSendCount, false)))
            case None =>
              println("processing first")
              fst.next(instructions).map { next =>
                (sendCount, (next.updateQueues(snd.sent), snd.updateQueues(next.sent), sendCount, true))
              }
          }
        }
      }
      .toVector
      .last
  }

  case class P1State(
      current: Int,
      playing: Option[Long],
      registers: Map[Char, Long]
  ) {
    def next(
        instructions: Vector[Instruction]
    ): Option[(Option[Long], P1State)] = {
      instructions.lift(current).flatMap {
        case Send(register)       => advance(playing = Some(registers(register)))
        case Set(target, operand) => update(target, operand, (_, b) => b)
        case Add(target, operand) => update(target, operand, _ + _)
        case Mul(target, operand) => update(target, operand, _ * _)
        case Mod(target, operand) => update(target, operand, _ % _)
        case Receive(register) =>
          if (registers(register) != 0) {
            None
          } else {
            advance()
          }
        case Jump(condition, operand) =>
          if (value(condition) > 0) {
            Some(playing -> copy(current = current + value(operand).toInt))
          } else {
            advance()
          }
      }
    }

    def advance(
        playing: Option[Long] = this.playing,
        registers: Map[Char, Long] = this.registers
    ): Option[(Option[Long], P1State)] = {
      Some(
        playing -> copy(
          current = current + 1,
          playing = playing,
          registers = registers
        )
      )
    }

    def value(regOrValue: RegOrValue): Long = regOrValue match {
      case Instruction.Register(r) => registers(r)
      case Instruction.Value(v)    => v
    }

    def update(
        register: Char,
        operand: RegOrValue,
        fn: (Long, Long) => Long
    ): Option[(Option[Long], P1State)] = {
      advance(registers =
        registers.updated(register, fn(registers(register), value(operand)))
      )
    }
  }

  case class P2State(
      registers: Map[Char, Long],
      currentInstruction: Int,
      sent: Option[Long],
      received: Vector[(Long, Int)]
  ) {
    def next(
        instructions: Vector[Instruction]
    ): Option[P2State] = {
      instructions.lift(currentInstruction).flatMap {
        case Send(register) =>
          Some(
            copy(
              sent = Some(registers(register)),
              currentInstruction = currentInstruction + 1
            )
          )
        case Set(target, operand) => Some(update(target, operand, (_, b) => b))
        case Add(target, operand) => Some(update(target, operand, _ + _))
        case Mul(target, operand) => Some(update(target, operand, _ * _))
        case Mod(target, operand) => Some(update(target, operand, _ % _))
        case Receive(register) =>
          received.headOption.map { case (msg, count) =>
            copy(
              registers = registers.updated(register, msg),
              currentInstruction = currentInstruction + 1,
              received = if (count == 1) {
                received.tail
              } else {
                received.updated(0, (msg, count - 1))
              }
            )
          }
        case Jump(condition, operand) =>
          if (value(condition) > 0) {
            Some(
              copy(currentInstruction =
                currentInstruction + value(operand).toInt
              )
            )
          } else {
            Some(copy(currentInstruction = currentInstruction + 1))
          }
      }
    }

    def value(regOrValue: RegOrValue): Long = regOrValue match {
      case Instruction.Register(r) => registers(r)
      case Instruction.Value(v)    => v
    }

    def update(
        register: Char,
        operand: RegOrValue,
        fn: (Long, Long) => Long
    ): P2State = {
      copy(
        registers =
          registers.updated(register, fn(registers(register), value(operand))),
        currentInstruction = currentInstruction + 1
      )
    }

    def updateQueues(nextReceived: Option[Long]): P2State = {
      val updated = nextReceived.map { msg =>
        received.lastOption match {
          case Some((`msg`, count)) => received.updated(received.length - 1, (msg, count + 1))
          case _ => received.appended((msg, 1))
        }
      }.getOrElse(received)
      copy(sent = None, received = updated)
    }
  }

  sealed trait Instruction
  case class Send(register: Char) extends Instruction
  case class Set(target: Char, operand: RegOrValue) extends Instruction
  case class Add(target: Char, operand: RegOrValue) extends Instruction
  case class Mul(target: Char, operand: RegOrValue) extends Instruction
  case class Mod(target: Char, operand: RegOrValue) extends Instruction
  case class Receive(register: Char) extends Instruction
  case class Jump(condition: RegOrValue, operand: RegOrValue) extends Instruction

  object Instruction {
    def parse(text: String): Instruction = {
      text.split(" ").toList match {
        case "snd" :: x :: Nil      => Send(x.head)
        case "set" :: x :: y :: Nil => Set(x.head, RegOrValue.parse(y))
        case "add" :: x :: y :: Nil => Add(x.head, RegOrValue.parse(y))
        case "mul" :: x :: y :: Nil => Mul(x.head, RegOrValue.parse(y))
        case "mod" :: x :: y :: Nil => Mod(x.head, RegOrValue.parse(y))
        case "rcv" :: x :: Nil      => Receive(x.head)
        case "jgz" :: x :: y :: Nil => Jump(RegOrValue.parse(x), RegOrValue.parse(y))
        case _ =>
          throw new IllegalArgumentException(s"Unexpected instruction $text")
      }
    }

    sealed trait RegOrValue {
      override def toString: String = this match {
        case Register(r) => r.toString
        case Value(v)    => v.toString
      }
    }
    case class Register(r: Char) extends RegOrValue
    case class Value(v: Int) extends RegOrValue
    object RegOrValue {
      def parse(text: String): RegOrValue = {
        text.toIntOption.map(Value).getOrElse(Register(text.head))
      }
    }
  }
}
