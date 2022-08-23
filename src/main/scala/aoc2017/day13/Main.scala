package aoc2017.day13

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """0: 3
                    |1: 2
                    |4: 4
                    |6: 4""".stripMargin
    val exampleFirewall = example.split("\n").map(Layer.parse)
    assert(exampleFirewall.map(l => l.severity(l.depth)).sum == 24)

    val part1Firewall = Source.fromResource("2017/13/part1.txt").getLines().map(Layer.parse)
    val part1 = part1Firewall.map(l => l.severity(l.depth)).sum
    println(s"part 1: $part1")
  }

  case class Layer(depth: Int, range: Int) {
    def severity(at: Int): Int = {
      // stupid concise code that requires a massive comment to explain:
      // e.g. for range = 3
      // - at % ((range - 1) * 2) to map into 0..4
      // - x - (range - 1) to map into -2..2
      // - x.abs to map into 2..0..2
      // - (range - 1) - x to map into 0..2..0
      val position = (range - 1) - (at % ((range - 1) * 2) - (range - 1)).abs
      if (position == 0) {
        depth * range
      } else {
        0
      }
    }
  }

  object Layer {
    def parse(line: String): Layer = {
      val regex = """(\d+): (\d+)""".r
      line match {
        case regex(depth, range) => Layer(depth.toInt, range.toInt)
        case _ => throw new IllegalArgumentException(s"Invalid firewall layer description $line")
      }
    }
  }
}
