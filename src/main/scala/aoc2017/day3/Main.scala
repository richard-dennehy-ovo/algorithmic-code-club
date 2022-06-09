package aoc2017.day3

import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    assert(spiral(0, 0) == 1)
    assert(spiral(1, 0) == 2)
    assert(spiral(1, 1) == 3)
    assert(spiral(0, 1) == 4)
    assert(spiral(-1, 1) == 5)
    assert(spiral(-1, 0) == 6)
    assert(spiral(-1, -1) == 7)
    assert(spiral(0, -1) == 8)
    assert(spiral(1, -1) == 9)
    assert(spiral(2, -1) == 10)
    assert(spiral(2, 0) == 11)
    assert(spiral(2, 1) == 12)
    assert(spiral(2, 2) == 13)
    assert(spiral(1, 2) == 14)
    assert(spiral(0, 2) == 15)
    assert(spiral(-1, 2) == 16)
    assert(spiral(-2, 2) == 17)
    assert(spiral(-2, 1) == 18)
    assert(spiral(-2, 0) == 19)
    assert(spiral(-2, -1) == 20)
    assert(spiral(-2, -2) == 21)
    assert(spiral(-1, -2) == 22)
    assert(spiral(0, -2) == 23)

    assert((0, 0) == coords(1))
    assert((1, 0) == coords(2))
    assert((1, 1) == coords(3))
    assert((0, 1) == coords(4))
    assert((-1, 1) == coords(5))
    assert((-1, 0) == coords(6))
    assert((-1, -1) == coords(7))
    assert((0, -1) == coords(8))
    assert((1, -1) == coords(9))
    assert((2, -1) == coords(10))
    assert((2, 0) == coords(11))
    assert((2, 1) == coords(12))
    assert((2, 2) == coords(13))
    assert((1, 2) == coords(14))
    assert((0, 2) == coords(15))
    assert((-1, 2) == coords(16))
    assert((-2, 2) == coords(17))
    assert((-2, 1) == coords(18))
    assert((-2, 0) == coords(19))
    assert((-2, -1) == coords(20))
    assert((-2, -2) == coords(21))
    assert((-1, -2) == coords(22))
    assert((0, -2) == coords(23))

    assert(distanceFromCentre(1) == 0)
    assert(distanceFromCentre(12) == 3)
    assert(distanceFromCentre(23) == 2)
    assert(distanceFromCentre(1024) == 31)

    println(s"Part 1: ${distanceFromCentre(289326)}")

    assert(spiralDistance(1, 0) == 0)
    assert(spiralDistance(1, 1) == 1)
    assert(spiralDistance(0, 1) == 2)
    assert(spiralDistance(-1, 1) == 3)
    assert(spiralDistance(-1, 0) == 4)
    assert(spiralDistance(-1, -1) == 5)
    assert(spiralDistance(0, -1) == 6)
    assert(spiralDistance(1, -1) == 7)

    assert(sumSpiral(0, 0) == 1)
    assert(sumSpiral(1, 0) == 1)
    assert(sumSpiral(1, 1) == 2)
    assert(sumSpiral(0, 1) == 4)
    assert(sumSpiral(-1, 1) == 5)
    assert(sumSpiral(-1, 0) == 10)
    assert(sumSpiral(-1, -1) == 11)
    assert(sumSpiral(0, -1) == 23)
    assert(sumSpiral(1, -1) == 25)
    assert(sumSpiral(2, -1) == 26)
    assert(sumSpiral(2, 0) == 54)
    assert(sumSpiral(2, 1) == 57)
    assert(sumSpiral(2, 2) == 59)
    assert(sumSpiral(1, 2) == 122)
    assert(sumSpiral(0, 2) == 133)
    assert(sumSpiral(-1, 2) == 142)
    assert(sumSpiral(-2, 2) == 147)
    assert(sumSpiral(-2, 1) == 304)
    assert(sumSpiral(-2, 0) == 330)
    assert(sumSpiral(-2, -1) == 351)
    assert(sumSpiral(-2, -2) == 362)
    assert(sumSpiral(-1, -2) == 747)
    assert(sumSpiral(0, -2) == 806)

    val p2 = Iterator.iterate(1)(_ + 1).find { square =>
      val (x, y) = coords(square)
      val sumSpiralValue = sumSpiral(x, y)
      sumSpiralValue > 289326
    }.map { square =>
      val (x, y) = coords(square)
      sumSpiral(x, y)
    }
    println(s"Part 2: $p2")
  }

  def spiral(x: Int, y: Int): Int = {
    val edge = x.abs.max(y.abs)
    val ringSize = edge * 2 + 1
    val ringMax = ringSize * ringSize

    if (y == -edge) {
      // bottom side
      ringMax - (edge - x)
    } else if (x == -edge) {
      // left side
      val bottomLeft = ringMax - (ringSize - 1)
      bottomLeft - (edge + y)
    } else if (y == edge) {
      // top side
      val topLeft = ringMax - (ringSize - 1) * 2
      topLeft - (edge + x)
    } else {
      // right side
      val topRight = ringMax - (ringSize - 1) * 3
      topRight - (edge - y)
    }
  }

  private val cache = mutable.Map.empty[(Int, Int), Int]

  def sumSpiral(x: Int, y: Int): Int = {
    if (cache.contains((x, y))) {
      cache((x, y))
    }
    if (x == 0 && y == 0) {
      1
    } else {
      def isBefore(other: (Int, Int)): Boolean = {
        val (otherX, otherY) = other
        val ringSize = x.abs.max(y.abs)
        val otherRingSize = otherX.abs.max(otherY.abs)
        val isInEarlierRing = ringSize > otherRingSize
        val isBeforeInSameRing =
          ringSize == otherRingSize && spiralDistance(x, y) > spiralDistance(otherX, otherY)

        isInEarlierRing || isBeforeInSameRing
      }

      val sum = Vector(
        (x - 1, y - 1),
        (x - 1, y),
        (x - 1, y + 1),
        (x, y - 1),
        (x, y + 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1)
      ).filter(isBefore)
        .map(cell => sumSpiral(cell._1, cell._2))
        .sum

      cache.put((x, y), sum)
      sum
    }
  }

  def spiralDistance(x: Int, y: Int): Int = {
    val edge = x.abs.max(y.abs)
    val ringSize = edge * 2 + 1

    if (y == -edge) {
      edge + (ringSize - 1) * 2 + (edge + x)
    } else if (x == -edge) {
      edge + (ringSize - 1) + (edge - y)
    } else if (y == edge) {
      edge + (edge - x)
    } else {
      y
    }
  }

  def distanceFromCentre(value: Int): Int = {
    val (x, y) = coords(value)
    x.abs + y.abs
  }

  def coords(value: Int): (Int, Int) = {
    val ringSize = {
      val nextSquare = Math.sqrt(value).ceil.toInt
      if (nextSquare % 2 == 0) {
        nextSquare + 1
      } else {
        nextSquare
      }
    }
    val edge = (ringSize - 1) / 2
    val bottomRight = ringSize * ringSize
    val bottomLeft = bottomRight - (ringSize - 1)
    val topLeft = bottomLeft - (ringSize - 1)
    val topRight = topLeft - (ringSize - 1)

    if (value <= topRight) {
      val diff = topRight - value
      (edge, edge - diff)
    } else if (value <= topLeft) {
      val diff = topLeft - value
      (diff - edge, edge)
    } else if (value <= bottomLeft) {
      val diff = bottomLeft - value
      (-edge, diff - edge)
    } else {
      val diff = bottomRight - value
      (edge - diff, -edge)
    }
  }

//  private def genSpiral(size: Int): Array[Array[Int]] = {
//    assert(size % 2 == 1)
//    val cells = Array.fill(size)(Array.fill(size)(0))
//    val centre = (size - 1) / 2
//    cells(centre)(centre) = 1
//
//    // while < size:
//    //    for each [right, up, left, down]:
//    //        for arm length:
//    //            set cell at position to n
//    //            increment n by 1
//    //            move position in direction
//    //    move position 1 down
//    //    increment arm length
//    (1 until size).foldLeft(((centre, centre), 2)) { case (((x, y), armLength), n) =>
//
//    }
//
//    cells
//  }
}
