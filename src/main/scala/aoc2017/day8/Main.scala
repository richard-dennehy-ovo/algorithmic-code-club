package aoc2017.day8

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("2017/8/part1.txt")
      .getLines()
      .map(Instruction.parse)
      .toVector
    val (finalState, largest) = input.foldLeft((Map.empty[String, Int].withDefaultValue(0), 0)) {
      case ((registers, maxValue), next) =>
        val state = next.execute(registers)
        (state, math.max(state.maxBy(_._2)._2, maxValue))
    }
    println(s"part 1: ${finalState.maxBy(_._2)}")
    println(s"part 2: $largest")
  }
}

case class Instruction(
    register: String,
    op: Op,
    value: Int,
    condition: Condition
) {
  def execute(registers: Map[String, Int]): Map[String, Int] = {
    if (condition.check(registers)) {
      op match {
        case Op.Inc => registers.updated(register, registers(register) + value)
        case Op.Dec => registers.updated(register, registers(register) - value)
      }
    } else {
      registers
    }
  }
}

object Instruction {
  def parse(line: String): Instruction = {
    val instructionRegex = """^(\S+) (\S+) (\S+) if (.*)$""".r
    line match {
      case instructionRegex(register, op, value, condition) =>
        Instruction(
          register,
          Op.parse(op),
          value.toInt,
          Condition.parse(condition)
        )
      case _ =>
        throw new IllegalArgumentException(s"Invalid instruction $line")
    }
  }
}

sealed trait Op

object Op {
  case object Inc extends Op
  case object Dec extends Op

  def parse(text: String): Op = {
    text match {
      case "inc" => Op.Inc
      case "dec" => Op.Dec
      case _     => throw new IllegalArgumentException(s"Invalid op $text")
    }
  }
}

case class Condition(register: String, comparator: Comparator, value: Int) {
  def check(registers: Map[String, Int]): Boolean = {
    val registerValue = registers(register)
    comparator match {
      case Comparator.Gt  => registerValue > value
      case Comparator.Gte => registerValue >= value
      case Comparator.Lt  => registerValue < value
      case Comparator.Lte => registerValue <= value
      case Comparator.Eq  => registerValue == value
      case Comparator.Ne  => registerValue != value
    }
  }
}

object Condition {
  def parse(text: String): Condition = {
    val conditionRegex = """^(\S+) (\S+) (\S+)$""".r
    text match {
      case conditionRegex(register, comparator, value) =>
        Condition(register, Comparator.parse(comparator), value.toInt)
      case _ =>
        throw new IllegalArgumentException(s"Invalid condition $text")
    }
  }
}

sealed trait Comparator

object Comparator {
  case object Gt extends Comparator
  case object Gte extends Comparator
  case object Lt extends Comparator
  case object Lte extends Comparator
  case object Eq extends Comparator
  case object Ne extends Comparator

  def parse(text: String): Comparator = {
    text match {
      case ">"  => Comparator.Gt
      case ">=" => Comparator.Gte
      case "<"  => Comparator.Lt
      case "<=" => Comparator.Lte
      case "==" => Comparator.Eq
      case "!=" => Comparator.Ne
    }
  }
}
