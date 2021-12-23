import InputReader._

import scala.util.matching.Regex
import scala.collection.mutable

object Day22 extends App {
  case class Cube(x: Int, y: Int, z: Int)
  sealed trait State
  case object On extends State
  case object Off extends State

  case class Step(
      state: State,
      xStart: Int,
      xEnd: Int,
      yStart: Int,
      yEnd: Int,
      zStart: Int,
      zEnd: Int
  )

  object Step {
    def fromString(str: String): Step = {
      val pattern: Regex =
        """(\w+) x=(-?)(\d+)..(-?)(\d+),y=(-?)(\d+)..(-?)(\d+),z=(-?)(\d+)..(-?)(\d+)""".r
      str match {
        case pattern(
              stateStr,
              xStartSignStr,
              xStartStr,
              xEndSignStr,
              xEndStr,
              yStartSignStr,
              yStartStr,
              yEndSignStr,
              yEndStr,
              zStartSignStr,
              zStartStr,
              zEndSignStr,
              zEndStr
            ) =>
          val state: State =
            stateStr match {
              case "on"  => On
              case "off" => Off
            }
          val xStart: Int = {
            if (xStartSignStr.size == 0) xStartStr.toInt
            else -1 * xStartStr.toInt
          }
          val xEnd: Int = {
            if (xEndSignStr.size == 0) xEndStr.toInt
            else -1 * xEndStr.toInt
          }
          val yStart: Int = {
            if (yStartSignStr.size == 0) yStartStr.toInt
            else -1 * yStartStr.toInt
          }
          val yEnd: Int = {
            if (yEndSignStr.size == 0) yEndStr.toInt
            else -1 * yEndStr.toInt
          }
          val zStart: Int = {
            if (zStartSignStr.size == 0) zStartStr.toInt
            else -1 * zStartStr.toInt
          }
          val zEnd: Int = {
            if (zEndSignStr.size == 0) zEndStr.toInt
            else -1 * zEndStr.toInt
          }
          Step(state, xStart, xEnd, yStart, yEnd, zStart, zEnd)
      }
    }
  }

  def solutionToFirstHalf(steps: List[Step]): Long = {
    def processStep(activeCubes: Set[Cube], step: Step): Set[Cube] = {
      val Step(state, xStart, xEnd, yStart, yEnd, zStart, zEnd) = step
      var currentActiveCubes: Set[Cube] = activeCubes

      for {
        x <- math.max(-50, xStart) to math.min(50, xEnd)
        y <- math.max(-50, yStart) to math.min(50, yEnd)
        z <- math.max(-50, zStart) to math.min(50, zEnd)
      } {
        val cube = Cube(x, y, z)
        state match {
          case On =>
            currentActiveCubes += cube
          case Off =>
            currentActiveCubes -= cube
        }
      }

      currentActiveCubes
    }

    var activeCubes = Set.empty[Cube]
    steps.foreach { step =>
      activeCubes = processStep(activeCubes, step)
    }

    activeCubes.size
  }

  def solutionToSecondHalf(steps: List[Step]): Long = {
    case class Cuboid(x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int) {
      def intersection(that: Cuboid): Option[Cuboid] = {
        def xOverlap: Boolean = !(that.x2 < x1 || x2 < that.x1)
        def yOverlap: Boolean = !(that.y2 < y1 || y2 < that.y1)
        def zOverlap: Boolean = !(that.z2 < z1 || z2 < that.z1)

        if (xOverlap && yOverlap && zOverlap) {
          val intersection =
            Cuboid(
              math.max(x1, that.x1),
              math.min(x2, that.x2),
              math.max(y1, that.y1),
              math.min(y2, that.y2),
              math.max(z1, that.z1),
              math.min(z2, that.z2)
            )
          Some(intersection)
        } else None
      }

      def volume: Long = {
        val x: Long = x2 - x1 + 1L
        val y: Long = y2 - y1 + 1L
        val z: Long = z2 - z1 + 1L

        x * y * z
      }
    }

    def processStep(
        step: Step,
        cuboidToMultiplier: Map[Cuboid, Int]
    ): Map[Cuboid, Int] = {
      var stepMap: mutable.Map[Cuboid, Int] = mutable.Map.empty[Cuboid, Int]
      stepMap ++= cuboidToMultiplier

      val Step(state, x1, x2, y1, y2, z1, z2) = step
      val stepCuboid = Cuboid(x1, x2, y1, y2, z1, z2)

      cuboidToMultiplier.foreach { case (cuboid, mult) =>
        cuboid.intersection(stepCuboid) match {
          case Some(intersection) =>
            stepMap(intersection) =
              stepMap.getOrElse(intersection, 0) + -1 * mult
          case None => // nothing to do
        }
      }

      if (state == On)
        stepMap(stepCuboid) = stepMap.getOrElse(stepCuboid, 0) + 1

      stepMap.toMap
    }

    var cuboidToMultiplier: Map[Cuboid, Int] = Map.empty[Cuboid, Int]

    for (step <- steps) {
      cuboidToMultiplier = processStep(step, cuboidToMultiplier)
    }

    cuboidToMultiplier.foldLeft(0L) {
      case (acc: Long, (cuboid: Cuboid, mult: Int)) =>
        acc + (cuboid.volume * mult)
    }
  }

  val inputSteps: List[Step] =
    readAllLines("day-22-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(Step.fromString)

  println(solutionToFirstHalf(inputSteps))
  println(solutionToSecondHalf(inputSteps))

}
