package aoc2022.day7

import aoc2022.day7.Main.Node.{Directory, File}

import scala.io.Source

object Main {
  private val DiskSpace = 70000000
  private val RequiredFreeSpace = 30000000

  def main(args: Array[String]): Unit = {
    val example = """$ cd /
                    |$ ls
                    |dir a
                    |14848514 b.txt
                    |8504156 c.dat
                    |dir d
                    |$ cd a
                    |$ ls
                    |dir e
                    |29116 f
                    |2557 g
                    |62596 h.lst
                    |$ cd e
                    |$ ls
                    |584 i
                    |$ cd ..
                    |$ cd ..
                    |$ cd d
                    |$ ls
                    |4060174 j
                    |8033020 d.log
                    |5626152 d.ext
                    |7214296 k""".stripMargin

    val parsed = example.linesIterator.map(CliOutput.parse).toVector

    val dir = traverse(parsed.iterator.buffered)

    val exampleP1 = dir.toVector.collect { case dir: Directory if dir.size <= 100000 => dir.size }.sum
    assert(exampleP1 == 95437)

    val actualInput = traverse(Source.fromResource("2022/7/p1.txt").getLines().map(CliOutput.parse).buffered)

    val p1 = actualInput.toVector.collect { case dir: Directory if dir.size <= 100000 => dir.size }.sum
    println(p1)
    assert(p1 == 1517599)

    val exampleUnused = DiskSpace - dir.size
    assert(exampleUnused == 21618835)
    val exampleToFree = RequiredFreeSpace - exampleUnused

    val exampleToDelete = dir.toVector.collect { case dir: Directory if dir.size >= exampleToFree => dir }.minBy(_.size)
    println(exampleToDelete)

    val p2Unused = DiskSpace - actualInput.size
    val p2ToFree = RequiredFreeSpace - p2Unused

    val p2ToDelete = actualInput.toVector.collect { case dir: Directory if dir.size >= p2ToFree => dir }.minBy(_.size)
    println(p2ToDelete.size)
  }

  private def traverse(instructions: scala.collection.BufferedIterator[CliOutput]): Directory = {
    // peek next
    // if `cd /`, consume
    // if empty, return
    // if non-empty, recursively:
    //  - peek next
    //  - if `cd /`, return
    //  - if `cd ..`, consume and return
    //  - if `ls`, consume and continue
    //  - if dir, ignore
    //  - if file, add to children
    //  - if `cd dir`, recursive call
    //  - add returned directory to children
    // - add returned directory to children

    def loop(current: Directory): Directory = {
      instructions.headOption match {
        case None =>
          current
        case Some(CliOutput.CdOut) =>
          instructions.next()
          current
        case Some(CliOutput.Ls) =>
          instructions.next()
          loop(current)
        case Some(CliOutput.DirectoryInfo(_)) =>
          instructions.next()
          loop(current)
        case Some(CliOutput.FileInfo(size, name)) =>
          instructions.next()
          loop(current.updated(name, File(name, size)))
        case Some(CliOutput.CdInto(name)) =>
          instructions.next()
          loop(current.updated(name, loop(Directory(name, Map.empty))))
        case Some(CliOutput.CdRoot) =>
          throw new IllegalStateException("Can't handle more than one `cd /`")
      }
    }

    assert(instructions.next() == CliOutput.CdRoot, "Instructions don't start with `cd /`")
    loop(Directory("/", Map.empty))
  }

  sealed trait CliOutput
  object CliOutput {
    case object CdRoot extends CliOutput
    case object Ls extends CliOutput
    case class DirectoryInfo(name: String) extends CliOutput
    case class FileInfo(size: Int, name: String) extends CliOutput
    case class CdInto(dirName: String) extends CliOutput
    case object CdOut extends CliOutput

    def parse(line: String): CliOutput = line match {
      case "$ cd /" => CdRoot
      case "$ cd .." => CdOut
      case ln if ln.startsWith("$ cd ") => CdInto(line.drop("$ cd ".length))
      case "$ ls" => Ls
      case ln if ln.startsWith("dir ") => DirectoryInfo(line.drop("dir ".length))
      case ln if ln.head.isDigit =>
        val size = ln.takeWhile(_.isDigit)
        FileInfo(size.toInt, ln.drop(size.length + 1))
      case _ => throw new IllegalArgumentException(s"Unparseable: $line")
    }
  }

  sealed trait Node {
    val size: Int
    val name: String

    def toVector: Vector[Node] = this match {
      case file: File => Vector(file)
      case dir@Directory(_, children) => Vector(dir) ++ children.flatMap(_._2.toVector)
    }
  }
  object Node {
    case class File(override val name: String, override val size: Int) extends Node
    case class Directory(override val name: String, children: Map[String, Node]) extends Node {
      override val size: Int = children.values.map(_.size).sum
      def updated(name: String, value: Node): Directory = Directory(this.name, children.updated(name, value))
    }
  }
}
