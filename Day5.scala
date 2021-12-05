import InputReader._
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}
import scala.collection.mutable

object Day5 extends App {
  case class Point(x: Int, y: Int)
  object Point {
    def fromString(str: String): Try[Point] =
      str.split(",").toList match {
        case x :: y :: Nil => Success(Point(x.toInt, y.toInt))
        case _ =>
          Failure(
            new IllegalArgumentException(
              s"$str cannot be converted to a Coordinate"
            )
          )
      }

  }

  case class Line(start: Point, end: Point)
  object Line {
    def fromString(str: String): Try[Line] =
      str.split(" -> ").toList match {
        case c1Str :: c2Str :: Nil =>
          for {
            c1 <- Point.fromString(c1Str)
            c2 <- Point.fromString(c2Str)
          } yield Line(c1, c2)
        case _ =>
          Failure(
            new IllegalArgumentException(s"$str cannot be converted to a Line")
          )
      }
  }

  def solutionToFirstHalf(lines: List[Line]): Int = {
    val pointToOccurence: mutable.Map[Point, Int] =
      mutable.Map.empty[Point, Int]

    lines.foreach { case Line(start, end) =>
      (start, end) match {
        case (Point(x1, y1), Point(x2, y2)) if (x1 == x2) =>
          (math.min(y1, y2) to math.max(y1, y2)).foreach { y =>
            val p = Point(x1, y)
            pointToOccurence(p) = pointToOccurence.getOrElse(p, 0) + 1
          }
        case (Point(x1, y1), Point(x2, y2)) if (y1 == y2) =>
          (math.min(x1, x2) to math.max(x1, x2)).foreach { x =>
            val p = Point(x, y1)
            pointToOccurence(p) = pointToOccurence.getOrElse(p, 0) + 1
          }

        case (Point(x1, y1), Point(x2, y2))
            if (math.abs(x1 - x2) == math.abs(y1 - y2)) =>
          val xStep = if ((x1 - x2) > 0) -1 else 1
          val yStep = if ((y1 - y2) > 0) -1 else 1
          var (x, y) = (x1, y1)

          while (x != x2 && y != y2) {
            val p = Point(x, y)
            pointToOccurence(p) = pointToOccurence.getOrElse(p, 0) + 1
            x = x + xStep
            y = y + yStep
          }

          val p = Point(x, y)
          pointToOccurence(p) = pointToOccurence.getOrElse(p, 0) + 1
        case _ => // do nothing
      }
    }

    pointToOccurence.count { case (_, occurence) =>
      occurence >= 2
    }
  }

  val inputLines: List[Line] =
    readAllLines("day-5-input.txt").map(_.strip).map(Line.fromString(_).get)

  println(solutionToFirstHalf(inputLines))
}
