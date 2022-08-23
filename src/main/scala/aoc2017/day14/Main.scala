package aoc2017.day14

object Main {
  def main(args: Array[String]): Unit = {
    def generators(seedA: Long, seedB: Long, factorA: Int = 1, factorB: Int = 1) = {
      Iterator
        .iterate(seedA)(n => n * 16807 % Int.MaxValue)
        .filter(_ % factorA == 0)
        .zip(Iterator.iterate(seedB)(n => n * 48271 % Int.MaxValue).filter(_ % factorB == 0))
        .drop(1)
    }
    def countMatches(gens: Iterator[(Long, Long)], iterations: Int) = {
      gens.take(iterations).count { case (a, b) =>
        a % 65536 == b % 65536
      }
    }

    val exampleGens = () => generators(65, 8921)
    assert(countMatches(exampleGens(), 5) == 1)
//    assert(countMatches(exampleGens(), 40 * 1000 * 1000) == 588)

    val actualGens = () => generators(883, 879)
    val part1 = countMatches(actualGens(), 40 * 1000 * 1000)
    println(s"part 1: $part1")

    val p2ExampleGens = () => generators(65, 8921, 4, 8)
    assert(countMatches(p2ExampleGens(), 5 * 1000 * 1000) == 309)

    val p2ActualGens = () => generators(883, 879, 4, 8)
    val part2 = countMatches(p2ActualGens(), 5 * 1000 * 1000)
    println(s"part 2: $part2")
  }
}
