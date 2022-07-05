package aoc2017.day7

import scala.collection.mutable
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    println(s"part 1: ${part1()}")
    println(s"part 2: ${part2()}")
  }

  def part1(): String = {
    val list =
      Source.fromResource("2017/7/part1.txt").getLines().map(Node.parse)
    val leaves = list.filter(_.children.nonEmpty).toVector
    val root = leaves.find(node =>
      leaves.forall(leaf => !leaf.children.contains(node.name))
    )
    root
      .map(_.name)
      .getOrElse(throw new IllegalStateException("solution not found"))
  }

  def part2(): Int = {
    val nodes =
      Source
        .fromResource("2017/7/part1.txt")
        .getLines()
        .map(Node.parse)
        .map(n => n.name -> n)
        .toMap

    val unbalancedNodes = nodes.filter { case (_, node) =>
      node.children
        .map(child => nodes(child).totalWeight(nodes))
        .distinct
        .length > 1
    }
    val (_, unbalancedNode) = unbalancedNodes.maxBy(_._2.depth(nodes))

    val weighted = unbalancedNode.children
      .map { child =>
        val node = nodes(child)
        node -> node.totalWeight(nodes)
      }
      .groupBy(_._2)
      .map { case (k, v) => k -> v.map(_._1) }

    val (actualWeight, unbalancedChild) = weighted.collectFirst {
      case (k, v) if v.length == 1 => k -> v.head
    }.head
    val expectedWeight = weighted.collectFirst {
      case (k, v) if v.length > 1 => k
    }.head
    unbalancedChild.weight - (actualWeight - expectedWeight)
  }

  val depthCache: mutable.Map[String, Int] = mutable.Map.empty

  case class Node(name: String, weight: Int, children: Vector[String]) {
    def depth(nodes: Map[String, Node]): Int = {
      depthCache.get(name) match {
        case Some(value) => value
        case None =>
          val depth = nodes.find(_._2.children.contains(name)) match {
            case Some((_, parent)) => parent.depth(nodes) + 1
            case None => 0
          }
          depthCache.put(name, depth)
          depth
      }
    }

    def totalWeight(nodes: Map[String, Node]): Int =
      weight + children.map(nodes(_).totalWeight(nodes)).sum
  }

  object Node {
    def parse(desc: String): Node = {
      val words = desc.split(" ")
      val name = words.head
      val weight = words(1).drop(1).dropRight(1).toInt
      val children = words.drop(3).map(_.stripSuffix(",")).toVector

      Node(name, weight, children)
    }
  }

}
