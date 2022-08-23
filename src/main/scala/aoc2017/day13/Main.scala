package aoc2017.day13

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """0: 3
                    |1: 2
                    |4: 4
                    |6: 4""".stripMargin
    val exampleFirewall = example.split("\n").map(Layer.parse)
    assert(exampleFirewall.map(l => l.severity(l.depth).getOrElse(0)).sum == 24)
    assert(
      Iterator.iterate(0)(_ + 1).find { n =>
        exampleFirewall.forall(l => l.severity(n + l.depth).isEmpty)
      }.get == 10
    )

    val part1Firewall = Source.fromResource("2017/13/part1.txt").getLines().map(Layer.parse).toVector
    val part1 = part1Firewall.map(l => l.severity(l.depth).getOrElse(0)).sum
    assert(part1 == 1904)
    println(s"part 1: $part1")

    val part2 = Iterator.iterate(0)(_ + 1).find { n =>
      part1Firewall.forall(l => l.severity(n + l.depth).isEmpty)
    }.get
    println(s"part 2: $part2")
  }

  case class Layer(depth: Int, range: Int) {
    def severity(at: Int): Option[Int] = {
      val scanInterval: Int = (range - 1) * 2
      if (at % scanInterval == 0) {
        Some(depth * range)
      } else {
        None
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
