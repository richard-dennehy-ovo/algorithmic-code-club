package aoc2016.day11

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("2016/11/part 1.txt")
      .getLines()
      .toVector

//    val initial = State.parse(input)
//    val state1 = initial.nextStates.head
//    val state2 = state1.nextStates.head
//    val state3 = state2.nextStates(5)
//    val state4 = state3.nextStates.head
//    val state5 = state4.nextStates.head
//    val state6 = state5.nextStates.head
//    val state7 = state6.nextStates(3)
//    val state8 = state7.nextStates(1)
//    val state9 = state8.nextStates.head
//    val state10 = state9.nextStates.head
//    val state11 = state10.nextStates.head

    val start = Instant.now

    // sort of breadth-first
    @tailrec
    def search(currentCandidates: Vector[State], futureCandidates: Vector[State], iteration: Int): State = {
      val (best, rest) = (currentCandidates.head, currentCandidates.tail)
      val elapsed = ChronoUnit.MILLIS.between(start, Instant.now)
      println(s"iteration $iteration: searching ${currentCandidates.size} candidates; chose g${best.generation}, w${best.weight}; elapsed ${elapsed.toFloat / 1000.0}s")
      if (best.weight == 0) {
        println(s"found in $iteration iterations")
        best
      } else {
        val next = best.nextStates
        if (rest.isEmpty) {
          val newCandidates = (futureCandidates ++ next).toArray
          java.util.Arrays.parallelSort(newCandidates, State.ordering)

          search(newCandidates.toVector, Vector.empty, iteration + 1)
        } else {
          search(rest, futureCandidates ++ next, iteration + 1)
        }
      }
    }

    val result = search(Vector(State.parse(input)), Vector.empty, 0)
    val elapsed = ChronoUnit.MILLIS.between(start, Instant.now)
    println(s"best: ${result.generation} in ${elapsed.toFloat / 1000.0} seconds")
  }
}

case class State(
    elevatorPosition: Int,
    floors: Vector[Vector[Component]],
    previousHashed: Int,
    generation: Int
) {
  val weight: Int = floors.zipWithIndex.map { case (components, floorNumber) =>
    components.length * (floors.length - (floorNumber + 1))
  }.sum

  def printDebugString: State = {
    floors.zipWithIndex.reverse.foreach { case (floor, floorNumber) =>
      print("F" + (floorNumber + 1))
      print(" ")
      if (floorNumber == elevatorPosition) {
        print("E")
      } else {
        print(".")
      }
      floor.foreach(component => print(' ' + component.debugString))
      println()
    }
    println()

    this
  }

  def isValid: Boolean = {
    floors.forall { components =>
      val (generators, microchips) = components.partitionMap(_.toEither)
      microchips.forall { chip =>
        generators.exists(_.element == chip.element) || generators.isEmpty
      }
    }
  }

  def nextStates: Vector[State] = {
    val singles = floors(elevatorPosition)
    val pairs = singles.combinations(2).toVector

    def move(to: Int, components: Vector[Component]): State = {
      val p = elevatorPosition
      this.copy(
        to,
        floors
          .updated(p, floors(p).filterNot(components.contains))
          .updated(to, floors(to) ++ components),
        hash,
        generation + 1
      )
    }

    val moveDown = if (elevatorPosition >= 1) {
      val moveOne = singles.map(c => move(elevatorPosition - 1, Vector(c)))
      val moveTwo = pairs.map(move(elevatorPosition - 1, _))

      moveOne ++ moveTwo
    } else {
      Vector.empty
    }

    val moveUp = if (elevatorPosition < (floors.length - 1)) {
      val moveOne = singles.map(c => move(elevatorPosition + 1, Vector(c)))
      val moveTwo = pairs.map(move(elevatorPosition + 1, _))

      moveOne ++ moveTwo
    } else {
      Vector.empty
    }

    (moveUp ++ moveDown)
      .filter(_.isValid)
      .filterNot(state => state.hash == previousHashed)
  }

  def hash: Int = (elevatorPosition, floors).hashCode()
}

object State {
  def apply(floors: Vector[Vector[Component]]): State =
    State(0, floors, 0, 0)

  def parse(input: Vector[String]): State = {
    val floors = input.map { description =>
      description
        .split(' ')
        .sliding(2)
        .flatMap {
          case Array(element, kind) if kind.startsWith("microchip") =>
            Some(Component.Microchip(element.dropRight("-compatible".length)))
          case Array(element, kind) if kind.startsWith("generator") =>
            Some(Component.Generator(element))
          case _ => None
        }
        .toVector
    }

    State(floors)
  }

  implicit val ordering: Ordering[State] = prioritiseGeneration

  lazy val prioritiseGeneration: Ordering[State] = Ordering.fromLessThan {
    case (s1, s2) if s1.generation < s2.generation => true
    case (s1, s2) if s1.generation == s2.generation && s1.weight < s2.weight => true
    case (s1, s2) if s1.generation == s2.generation && s1.weight == s2.weight => s1.## < s2.##
    case _ => false
  }

  lazy val prioritiseWeight: Ordering[State] = Ordering.fromLessThan {
    case (s1, s2) if s1.weight < s2.weight => true
    case (s1, s2) if s1.weight == s2.weight && s1.generation < s2.generation => true
    case (s1, s2) if s1.weight == s2.weight && s1.generation == s2.generation => s1.## < s2.##
    case _ => false
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
