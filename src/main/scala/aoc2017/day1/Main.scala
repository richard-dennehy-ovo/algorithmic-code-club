package aoc2017.day1

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    assert(captchaSum("1122") == 3)
    assert(captchaSum("1111") == 4)
    assert(captchaSum("1234") == 0)
    assert(captchaSum("91212129") == 9)

    println(s"part 1: ${part1()}")

    assert(captchaSum2("1212") == 6)
    assert(captchaSum2("1221") == 0)
    assert(captchaSum2("123425") == 4)
    assert(captchaSum2("123123") == 12)
    assert(captchaSum2("12131415") == 4)

    println(s"part 2: ${part2()}")
  }

  private def part1(): Int = {
    Source.fromResource("2017/1/part 1.txt")
      .getLines()
      .take(1)
      .map(captchaSum)
      .next()
  }

  private def part2(): Int = {
    Source.fromResource("2017/1/part 1.txt")
      .getLines()
      .take(1)
      .map(captchaSum2)
      .next()
  }

  private def captchaSum(captcha: String): Int = {
    val leading = captcha
      .iterator
      .sliding(2)
      .map {
        case Seq(c1, c2) if c1.isDigit && c1 == c2 =>
          digitToInt(c1)
        case _ => 0
      }.sum

    val last = if (captcha.last.isDigit && captcha.last == captcha.head) {
      digitToInt(captcha.last)
    } else {
      0
    }

    leading + last
  }

  private def captchaSum2(captcha: String): Int = {
    captcha.iterator.zipWithIndex.map { case (char, index) =>
      val comparator = captcha((index + (captcha.length / 2)) % captcha.length)
      if (char.isDigit && char == comparator) {
        digitToInt(char)
      } else {
        0
      }
    }.sum
  }

  private def digitToInt(digit: Char): Int = {
    digit.toInt - '0'.toInt
  }
}
