import Component.{Generator, Microchip}

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("2016/11/part 1.txt")
      .getLines()
      .toVector

    val start = Instant.now
//    val s1 = State(2, HashMap(Generator("lithium") -> 0, Generator("hydrogen") -> 3), HashMap(Microchip("lithium") -> 0, Microchip("hydrogen") -> 3), HashSet(10), 0)
//    val s2 = State(2, HashMap(Generator("lithium") -> 3, Generator("hydrogen") -> 0), HashMap(Microchip("lithium") -> 3, Microchip("hydrogen") -> 0), HashSet.empty, 10)
//    assert(s1.hash == s2.hash)

    // sort of breadth-first
    @tailrec
    def search(
        currentCandidates: HashMap[Int, State],
        futureCandidates: HashMap[Int, State],
        iteration: Int
    ): State = {
      val (best, rest) = (currentCandidates.head._2, currentCandidates.tail)
      val elapsed = ChronoUnit.MILLIS.between(start, Instant.now)
      println(
        s"iteration $iteration: searching ${currentCandidates.size} candidates; chose g${best.generation}, w${best.minimumMovesRemaining}; elapsed ${elapsed.toFloat / 1000.0}s"
      )
      val next = best.nextStates
      next.find(_._2.minimumMovesRemaining == 0) match {
        case Some((_, solution)) =>
          println(s"found in ${iteration + 1} iterations")
          solution
        case None if rest.isEmpty =>
          search(futureCandidates ++ next, HashMap.empty, iteration + 1)
        case None =>
          search(rest, futureCandidates ++ next, iteration + 1)
      }
    }

    val state = State.parse(input)
    val part2State = state.copy(
      generators = state.generators ++ HashMap(
        Generator("elerium") -> 0,
        Generator("dilithium") -> 0
      ),
      microchips = state.microchips ++ HashMap(
        Microchip("elerium") -> 0,
        Microchip("dilithium") -> 0
      )
    )
    // iteration 5720003: searching 1496972 candidates; chose g31, w0; elapsed 327.502s
    // iteration 3902: searching 82 candidates; chose g30, w2; elapsed 0.531s
    val result =
      search(HashMap(part2State.hash -> part2State), HashMap.empty, 0)
    val elapsed = ChronoUnit.MILLIS.between(start, Instant.now)
    println(
      s"best: ${result.generation} in ${elapsed.toFloat / 1000.0} seconds"
    )
  }
}

case class State(
    elevatorPosition: Int,
    generators: HashMap[Component.Generator, Int],
    microchips: HashMap[Component.Microchip, Int],
    previousHashes: HashSet[Int],
    generation: Int
) {
  val pairs: Vector[Vector[String]] = {
    (0 to 3).map { floor =>
      generators.collect {
        case (generator, `floor`)
            if microchips(Microchip(generator.element)) == floor =>
          generator.element
      }.toVector
    }.toVector
  }

  val minimumMovesRemaining: Int = {
    val g = generators.map { case (_, floorNumber) =>
      3 - floorNumber
    }.sum
    val m = microchips.map { case (_, floorNumber) =>
      3 - floorNumber
    }.sum

    g + m
  }

  def isValid: Boolean = {
    !previousHashes.contains(this.hash) &&
    microchips.forall { case (chip, floor) =>
      val gs = generators.filter(_._2 == floor)
      gs.isEmpty || gs.contains(Component.Generator(chip.element))
    }
  }

  def nextStates: Map[Int, State] = {
    def newState(
        to: Int,
        generators: HashMap[Generator, Int] = this.generators,
        microchips: HashMap[Microchip, Int] = this.microchips
    ): Option[State] = {
      Some(
        copy(
          to,
          generators,
          microchips,
          previousHashes + this.hash,
          generation + 1
        )
      ).filter(_.isValid)
    }

    def moveChip(to: Int, microchip: Microchip): Option[State] = {
      newState(
        to,
        microchips = microchips.updatedWith(microchip)(_.map(_ => to))
      )
    }

    def moveGenerator(to: Int, generator: Generator): Option[State] = {
      newState(
        to,
        generators = generators.updatedWith(generator)(_.map(_ => to))
      )
    }

    def moveChips(to: Int, m1: Microchip, m2: Microchip): Option[State] = {
      newState(
        to,
        microchips = microchips
          .updatedWith(m1)(_.map(_ => to))
          .updatedWith(m2)(_.map(_ => to))
      )
    }

    def moveGenerators(to: Int, g1: Generator, g2: Generator): Option[State] = {
      newState(
        to,
        generators = generators
          .updatedWith(g1)(_.map(_ => to))
          .updatedWith(g2)(_.map(_ => to))
      )
    }

    def movePair(
        to: Int,
        generator: Generator,
        microchip: Microchip
    ): Option[State] = {
      newState(
        to,
        generators.updatedWith(generator)(_.map(_ => to)),
        microchips.updatedWith(microchip)(_.map(_ => to))
      )
    }

    val chips = microchips.filter(_._2 == elevatorPosition)
    val gens = generators.filter(_._2 == elevatorPosition)

    val moveDown = if (elevatorPosition >= 1) {
      // stolen from reddit: moving any one chip is equivalent to moving any other chip
      val moveOneChip = chips.keys
        .flatMap(moveChip(elevatorPosition - 1, _))
        .take(1)
      val moveTwoChips = chips.keys.toVector
        .combinations(2)
        .flatMap(cs => moveChips(elevatorPosition - 1, cs(0), cs(1)))
      val moveOneGenerator =
        gens.keys.flatMap(moveGenerator(elevatorPosition - 1, _))
      val moveTwoGenerators = gens.keys.toVector
        .combinations(2)
        .flatMap(gs => moveGenerators(elevatorPosition - 1, gs(0), gs(1)))
      val movePaired = pairs(elevatorPosition).flatMap(element =>
        movePair(elevatorPosition - 1, Generator(element), Microchip(element))
      )

      val moveOne = (moveOneChip ++ moveOneGenerator).toVector
      val moveTwo = (moveTwoChips ++ moveTwoGenerators ++ movePaired).toVector

      // stolen from reddit: moving one down is strictly better than moving two down
      if (moveOne.isEmpty) { moveTwo }
      else { moveOne }
    } else {
      Vector.empty
    }

    val moveUp = if (elevatorPosition < 3) {
      val moveOneChip = chips.keys
        .flatMap(moveChip(elevatorPosition + 1, _))
        .take(1)
      val moveTwoChips = chips.keys.toVector
        .combinations(2)
        .flatMap(cs => moveChips(elevatorPosition + 1, cs(0), cs(1)))
      val moveOneGenerator =
        gens.keys.flatMap(moveGenerator(elevatorPosition + 1, _))
      val moveTwoGenerators = gens.keys.toVector
        .combinations(2)
        .flatMap(gs => moveGenerators(elevatorPosition + 1, gs(0), gs(1)))
      val movePaired = pairs(elevatorPosition).flatMap(element =>
        movePair(elevatorPosition + 1, Generator(element), Microchip(element))
      )

      val moveOne = (moveOneChip ++ moveOneGenerator).toVector
      val moveTwo = (moveTwoChips ++ moveTwoGenerators ++ movePaired).toVector

      // stolen from reddit: moving two up is strictly better than moving one up
      if (moveTwo.isEmpty) { moveOne }
      else { moveTwo }
    } else {
      Vector.empty
    }

    (moveUp ++ moveDown).map(state => state.hash -> state).toMap
  }

  def hash: Int = {
    // the actual elements don't matter, just their locations
    val pairsPerFloor = pairs.map(_.length)
    val unpairedGensPerFloor = generators.foldLeft(Vector(0, 0, 0, 0)) {
      case (counts, (_, floor)) =>
        counts.updated(floor, counts(floor) + 1)
    }
    val unpairedElementsPerFloor = generators.foldLeft(Vector(0, 0, 0, 0)) {
      case (counts, (_, floor)) =>
        counts.updated(floor, counts(floor) + 1)
    }

    (
      pairsPerFloor,
      unpairedGensPerFloor,
      unpairedElementsPerFloor,
      elevatorPosition
    ).##
  }
}

object State {
  def parse(input: Vector[String]): State = {
    val (generators, microchips) = (
      collection.mutable.HashMap.empty[Generator, Int],
      collection.mutable.HashMap.empty[Microchip, Int]
    )

    input.zipWithIndex.foreach { case (description, floor) =>
      description
        .split(' ')
        .sliding(2)
        .foreach {
          case Array(element, kind) if kind.startsWith("microchip") =>
            microchips.addOne(
              Microchip(element.dropRight("-compatible".length)) -> floor
            )
          case Array(element, kind) if kind.startsWith("generator") =>
            generators.addOne(Generator(element) -> floor)
          case _ => ()
        }
    }

    State(
      0,
      HashMap.from(generators),
      HashMap.from(microchips),
      HashSet.empty,
      0
    )
  }

  implicit val ordering: Ordering[State] = hybrid

  lazy val prioritiseGeneration: Ordering[State] = Ordering.fromLessThan {
    case (s1, s2) if s1.generation < s2.generation => true
    case (s1, s2)
        if s1.generation == s2.generation && s1.minimumMovesRemaining < s2.minimumMovesRemaining =>
      true
    case (s1, s2)
        if s1.generation == s2.generation && s1.minimumMovesRemaining == s2.minimumMovesRemaining =>
      s1.## < s2.##
    case _ => false
  }

  lazy val prioritiseWeight: Ordering[State] = Ordering.fromLessThan {
    case (s1, s2) if s1.minimumMovesRemaining < s2.minimumMovesRemaining => true
    case (s1, s2)
        if s1.minimumMovesRemaining == s2.minimumMovesRemaining && s1.generation < s2.generation =>
      true
    case (s1, s2)
        if s1.minimumMovesRemaining == s2.minimumMovesRemaining && s1.generation == s2.generation =>
      s1.## < s2.##
    case _ => false
  }

  lazy val hybrid: Ordering[State] = Ordering.fromLessThan { case (s1, s2) =>
    if (
      s1.generation == s2.generation && s1.minimumMovesRemaining < s2.minimumMovesRemaining
    ) {
      true
    } else if (s1.generation > s2.generation) {
      s1.minimumMovesRemaining < s2.minimumMovesRemaining && (s2.minimumMovesRemaining - s1.minimumMovesRemaining) >= (s1.generation - s2.generation) * 2
    } else if (s1.generation < s2.generation) {
      (s1.minimumMovesRemaining - s2.minimumMovesRemaining) < (s2.generation - s1.generation) * 2
    } else {
      s1.generation == s2.generation && s1.minimumMovesRemaining == s2.minimumMovesRemaining && s1.## < s2.##
    }
  }
}

sealed trait Component {
  def debugString: String = this match {
    case Component.Generator(element) => element.head.toUpper + "G"
    case Component.Microchip(element) => element.head.toUpper + "M"
  }

  def toEither: Either[Component.Generator, Component.Microchip] = this match {
    case generator: Component.Generator => Left(generator)
    case microchip: Component.Microchip => Right(microchip)
  }
}

object Component {
  case class Generator(element: String) extends Component
  case class Microchip(element: String) extends Component
}
