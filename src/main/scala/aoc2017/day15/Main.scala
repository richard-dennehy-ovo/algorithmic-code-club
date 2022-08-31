package aoc2017.day15

object Main {
  def main(args: Array[String]): Unit = {
    def generator(seed: Long, multiplier: Long): Iterator[Long] = Iterator.iterate(seed)(n => n * multiplier % Int.MaxValue).drop(1)
    val generatorAGenerator = generator(_, 16807)
    val generatorBGenerator = generator(_, 48271)

    def countMatches(gens: Iterator[(Long, Long)], iterations: Int) = {
      gens.take(iterations).count { case (a, b) =>
        a % 65536 == b % 65536
      }
    }

    val exampleGens = () => generatorAGenerator(65).zip(generatorBGenerator(8921))
    assert(countMatches(exampleGens(), 5) == 1)
//    assert(countMatches(exampleGens(), 40 * 1000 * 1000) == 588)

    val actualGens = () => generatorAGenerator(883).zip(generatorBGenerator(879))
    val part1 = countMatches(actualGens(), 40 * 1000 * 1000)
    println(s"part 1: $part1")

    def filteredAGenerator(seed: Long) = generatorAGenerator(seed).filter(_ % 4 == 0)
    def filteredBGenerator(seed: Long) = generatorBGenerator(seed).filter(_ % 8 == 0)

    val p2ExampleGens = () => filteredAGenerator(65).zip(filteredBGenerator(8921))
    assert(countMatches(p2ExampleGens(), 5 * 1000 * 1000) == 309)

    val p2ActualGens = () => filteredAGenerator(883).zip(filteredBGenerator(879))
    val part2 = countMatches(p2ActualGens(), 5 * 1000 * 1000)
    println(s"part 2: $part2")
  }
}
