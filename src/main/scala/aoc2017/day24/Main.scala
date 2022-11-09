package aoc2017.day24

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example = """0/2
                    |2/2
                    |2/3
                    |3/4
                    |3/5
                    |0/1
                    |10/1
                    |9/10""".stripMargin.linesIterator.map(Component.parse).toVector
    println(findStrongest(example))

    val part1 = Source.fromResource("2017/24/part1.txt").getLines().map(Component.parse).toVector
    val strongestP1 = findStrongest(part1)
    println(strongestP1)
    println(strongestP1.map(_.strength).sum)

    println(findLongest(example))
    val longestP1 = findLongest(part1)
    println(longestP1)
    println(longestP1.map(_.strength).sum)
  }

  def findStrongest(components: Vector[Component]): Vector[Component] = {
    def strongestFrom(openPort: Int, unused: Vector[Component]): Vector[Component] = {
      val options = unused.filter(_.contains(openPort))
      if (options.isEmpty) {
        Vector.empty
      } else {
        options.map { c =>
          val open = if (c.first == openPort) c.second else c.first
          Vector(c) ++ strongestFrom(open, unused.filterNot(_ == c))
        }.maxBy(_.map(_.strength).sum)
      }
    }

    strongestFrom(0, components)
  }

  def findLongest(components: Vector[Component]): Vector[Component] = {
    def longestFrom(openPort: Int, unused: Vector[Component]): Vector[Component] = {
      val options = unused.filter(_.contains(openPort))
      if (options.isEmpty) {
        Vector.empty
      } else {
        options.map { c =>
          val open = if (c.first == openPort) c.second else c.first
          Vector(c) ++ longestFrom(open, unused.filterNot(_ == c))
        }.sortWith((fst, snd) => fst.length < snd.length || fst.map(_.strength).sum < snd.map(_.strength).sum).last
      }
    }

    longestFrom(0, components)
  }

  case class Component(first: Int, second: Int) {
    def contains(port: Int): Boolean = first == port || second == port
    val strength: Int = first + second
  }
  object Component {
    def parse(text: String): Component = {
      val parts = text.split('/')
      assert(parts.length == 2)
      Component(parts(0).toInt, parts(1).toInt)
    }
  }
}
