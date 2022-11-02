package aoc2017.day23

import scala.io.Source
import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

object Main {
  def main(args: Array[String]): Unit = {
    val instructions = Source
      .fromResource("2017/23/part1.txt")
      .getLines()
      .map(Instruction.parse)
      .toVector

    val p1 = Iterator
      .iterate(Option(State('a' to 'h' map (_ -> 0L) toMap, 0, 0)))(
        _.flatMap(_.next(instructions))
      )
      .takeWhile(_.isDefined)
      .toVector
      .last
    p1.foreach(println)
    println(translatedUnoptimised(0))
    println(translatedOptimised(1))
  }

  case class State(
      registers: Map[Char, Long],
      currentInstruction: Int,
      mulsExecuted: Int
  ) {
    def next(instructions: Vector[Instruction]): Option[State] =
      instructions.lift(currentInstruction).map {
        case Instruction.Set(target, operand) =>
          update(target, operand, (_, b) => b)
        case Instruction.Sub(target, operand) =>
          update(target, operand, (a, b) => a - b)
        case Instruction.Mul(target, operand) =>
          update(target, operand, (a, b) => a * b)
            .copy(mulsExecuted = mulsExecuted + 1)
        case Instruction.Jump(condition, operand) =>
          if (value(condition) != 0) {
            copy(currentInstruction = currentInstruction + value(operand).toInt)
          } else {
            copy(currentInstruction = currentInstruction + 1)
          }
      }

    def value(regOrValue: RegOrValue): Long = regOrValue match {
      case Register(r) => registers(r)
      case Value(v)    => v
    }

    def update(
        register: Char,
        operand: RegOrValue,
        fn: (Long, Long) => Long
    ): State = {
      copy(
        registers =
          registers.updated(register, fn(registers(register), value(operand))),
        currentInstruction = currentInstruction + 1
      )
    }
  }

  sealed trait Instruction

  object Instruction {
    case class Set(target: Char, operand: RegOrValue) extends Instruction

    case class Sub(target: Char, operand: RegOrValue) extends Instruction

    case class Mul(target: Char, operand: RegOrValue) extends Instruction

    case class Jump(condition: RegOrValue, operand: RegOrValue)
        extends Instruction

    def parse(text: String): Instruction = {
      text.split(" ").toList match {
        case "set" :: x :: y :: Nil => Set(x.head, RegOrValue.parse(y))
        case "sub" :: x :: y :: Nil => Sub(x.head, RegOrValue.parse(y))
        case "mul" :: x :: y :: Nil => Mul(x.head, RegOrValue.parse(y))
        case "jnz" :: x :: y :: Nil =>
          Jump(RegOrValue.parse(x), RegOrValue.parse(y))
        case _ =>
          throw new IllegalArgumentException(s"Unexpected instruction $text")
      }
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

  def translatedUnoptimised(aInit: Int): (Int, Int) = {
    var b = 0
    var c = 0
    var d = 0
    var e = 0
    var f = 0
    var g = 0
    var h = 0
    var muls = 0

    b = 81
    c = b
    if (aInit != 0) {
      muls += 1
      b *= 100
      b += 100000
      c = b
      c += 17000
    }
    breakable {
      while(true) {
        f = 1
        d = 2
        do {
          e = 2
          do {
            g = d
            muls += 1
            g *= e
            g -= b
            if (g == 0) {
              f = 0
            }
            e += 1
            g = e
            g -= b
          } while (g != 0)
          d += 1
          g = d
          g -= b
        } while (g != 0)
        if (f == 0) {
          h += 1
        }
        g = b
        g -= c
        if (g == 0) {
          break()
        } else {
          b += 17
        }
      }
    }

    (muls, h)
  }

  def translatedOptimised(aInit: Int): Int = {
    var value = 0
    var end = 0
    var divisor = 0
    var prime = false
    var nonPrimes = 0

    value = 81
    end = value
    if (aInit != 0) {
      value *= 100
      value += 100000
      end = value
      end += 17000
    }
    var loop = true
    while (loop) {
      prime = true
      divisor = 2
      do {
        prime = value % divisor != 0
        divisor += 1
      } while (divisor * 2 <= value && prime)
      if (!prime) {
        nonPrimes += 1
      }
      if (value == end) {
        loop = false
      } else {
        value += 17
      }
    }

    nonPrimes
  }
}
