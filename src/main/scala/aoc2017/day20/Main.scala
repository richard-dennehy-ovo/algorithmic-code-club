package aoc2017.day20

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val example =
      Particle
        .parseMany("""p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
        |p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>
        |""".stripMargin.linesIterator)
        .min

    println(example.index)

    val particles =
      Particle.parseMany(Source.fromResource("2017/20/part1.txt").getLines())
    val part1 = particles.min
    println(part1.index)
    println(p2IterateSolve(particles))
  }

  def p2IterateSolve(particles: Vector[Particle]): Int = {
    val maxTimestep = 1000
    (0 to maxTimestep).foldLeft(particles) { case (particles, step) =>
      particles.groupBy(_.at(step)).values.collect {
        case p if p.length == 1 => p.head
      }.toVector
    }.length
  }

  case class V3(x: Long, y: Long, z: Long) {
    val distance: Long = x.abs + y.abs + z.abs
    def +(rhs: V3): V3 = V3(x + rhs.x, y + rhs.y, z + rhs.z)
    def *(rhs: Long): V3 = V3(x * rhs, y * rhs, z * rhs)
    def /(rhs: Long): V3 = V3(x / rhs, y / rhs, z / rhs)
  }

  object V3 {
    def parse(text: String): V3 = {
      val regex = raw"< *(-?\d+), *(-?\d+), *(-?\d+)>".r
      text match {
        case regex(x, y, z) => V3(x.toLong, y.toLong, z.toLong)
        case _ =>
          throw new IllegalArgumentException(s"Cannot parse $text as V3")
      }
    }
  }

  case class Particle(
      index: Int,
      start: V3,
      velocity: V3,
      acceleration: V3
  ) {
    def at(time: Long): V3 =
      (acceleration * time * (time + 1) / 2) + velocity * time + start
  }

  object Particle {
    def parseMany(lines: Iterator[String]): Vector[Particle] = {
      lines.zipWithIndex.map((parse _).tupled).toVector
    }

    def parse(text: String, index: Int): Particle = {
      val regex = raw"p=(.*?), v=(.*?), a=(.*?)".r
      text match {
        case regex(p, v, a) =>
          Particle(index, V3.parse(p), V3.parse(v), V3.parse(a))
        case _ =>
          throw new IllegalArgumentException(s"Cannot parse $text as Particle")
      }
    }

    implicit val ordering: Ordering[Particle] = Ordering
      .by { p: Particle => p.acceleration.distance }
      .orElse(Ordering.by(_.velocity.distance))
      .orElse(Ordering.by(_.start.distance))
  }
}
