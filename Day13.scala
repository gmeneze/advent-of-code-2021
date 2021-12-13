import InputReader._

import scala.util.{Try, Success, Failure}

object Day13 extends App {
  sealed trait Input
  object Input {
    case class Dot(x: Int, y: Int) extends Input

    sealed trait Instruction extends Input
    object Instruction {
      case class FoldUp(y: Int) extends Instruction
      case class FoldLeft(x: Int) extends Instruction
    }

    import Instruction._

    def fromString(str: String): Try[Input] = Try {
      val dotPattern = """(\d+)\,(\d+)""".r
      val foldUpPattern = """fold along y=(\d+)""".r
      val foldLeftPattern = """fold along x=(\d+)""".r
      str match {
        case dotPattern(x, y)   => Dot(x.toInt, y.toInt)
        case foldUpPattern(y)   => FoldUp(y.toInt)
        case foldLeftPattern(x) => FoldLeft(x.toInt)
        case _ =>
          throw new IllegalArgumentException(s"cannot parse: $str into Input")
      }
    }
  }

  import Input._
  import Instruction._

  def processInstruction(dots: Set[Dot], instruction: Instruction): Set[Dot] = {
    dots.map { case dot =>
      instruction match {
        case FoldUp(y) =>
          if (dot.y > y) Dot(dot.x, 2 * y - dot.y)
          else dot
        case FoldLeft(x) =>
          if (dot.x > x) Dot(2 * x - dot.x, dot.y)
          else dot
      }
    }
  }

  def solutionToFirstPart(
      dots: Set[Dot],
      instructions: List[Instruction]
  ): Int = {
    val firstInstruction: Instruction = instructions.head

    processInstruction(dots, firstInstruction).size
  }

  def visualize(dotSet: Set[Dot]): Unit = {
    val xMax = dotSet.map { case Dot(x, y) => x }.max
    val yMax = dotSet.map { case Dot(x, y) => y }.max

    var board: Vector[Vector[String]] =
      (0 to (yMax + 1)).map { _ =>
        Vector.fill(xMax + 1)(".")
      }.toVector

    dotSet.foreach { case Dot(x, y) =>
      val row = board(y)
      board = board.updated(y, row.updated(x, "#"))
    }

    board.foreach { row =>
      println(row.mkString(""))
    }
  }

  def solutionToSecondPart(
      dots: Set[Dot],
      instructions: List[Instruction]
  ): Unit = {
    var dotSet = dots

    instructions.foreach { instruction =>
      dotSet = processInstruction(dotSet, instruction)
    }

    visualize(dotSet)
  }

  val (dots: Set[Dot], instructions: List[Instruction]) = {
    val (dots: List[Dot], instructions: List[Instruction]) =
      readAllLines("day-13-input.txt")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .map(fromString(_).get)
        .partition {
          case Dot(_, _)   => true
          case FoldUp(_)   => false
          case FoldLeft(_) => false
        }
    (dots.toSet, instructions)
  }

  println(solutionToFirstPart(dots, instructions))
  solutionToSecondPart(dots, instructions)
}
