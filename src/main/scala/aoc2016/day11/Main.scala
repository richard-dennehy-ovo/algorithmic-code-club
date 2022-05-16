package aoc2016.day11

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("2016/11/example.txt")
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

    // attempt A*
    @tailrec
    def search(candidates: Vector[State]): State = {
      println(s"searching ${candidates.length} candidates")
      val best = candidates.head
      if (best.weight == 0) {
        best
      } else {
        val next = best.nextStates
        val nextCandidates = candidates.drop(1) ++ next
        val sorted = nextCandidates.sortWith { case (s1, s2) =>
          s1.generation < s2.generation || (s1.generation == s2.generation && s1.weight < s2.weight)
        }
        search(sorted)
      }
    }

    val result = search(Vector(State.parse(input)))
    println(result.generation)
  }
}

case class State(
    elevatorPosition: Int,
    floors: Vector[Vector[Component]],
    previous: HashSet[Int],
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
        previous + hash,
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
      .filterNot(state => previous.contains(state.hash))
      .sortBy(_.weight)
  }

  def hash: Int = (elevatorPosition, floors).hashCode()
}

object State {
  def apply(floors: Vector[Vector[Component]]): State =
    State(0, floors, HashSet.empty, 0)

  def parse(input: Vector[String]): State = {
    val floors = input.map { description =>
      description
        .dropRight(1)
        .split(' ')
        .sliding(2)
        .flatMap {
          case Array(element, "microchip") =>
            Some(Component.Microchip(element.dropRight("-compatible".length)))
          case Array(element, "generator") => Some(Component.Generator(element))
          case _                           => None
        }
        .toVector
    }

    State(floors)
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
