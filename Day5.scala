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

  case class Line(start: Point, end: Point) {
    def sameX: Boolean = start.x == end.x
    def sameY: Boolean = start.y == end.y
    def isDiagonal: Boolean =
      math.abs(start.x - end.x) == math.abs(start.y - end.y)

    def allPoints: List[Point] = {
      (start, end) match {
        case (Point(x1, y1), Point(x2, y2)) if sameX =>
          val yStep = if ((y1 - y2) > 0) -1 else 1
          (y1 to y2 by yStep).map(y => Point(x1, y)).toList
        case (Point(x1, y1), Point(x2, y2)) if sameY =>
          val xStep = if ((x1 - x2) > 0) -1 else 1
          (x1 to x2 by xStep).map(x => Point(x, y1)).toList
        case (Point(x1, y1), Point(x2, y2)) if isDiagonal =>
          val xStep = if ((x1 - x2) > 0) -1 else 1
          val yStep = if ((y1 - y2) > 0) -1 else 1
          (x1 to x2 by xStep).zip(y1 to y2 by yStep).map(Point.apply).toList
        case _ => Nil
      }
    }
  }

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
    val sameXOrYLines = lines.filter(line => line.sameX || line.sameY)

    sameXOrYLines
      .flatMap(_.allPoints)
      .groupBy(identity)
      .map { case (p, points) => p -> points.size }
      .count(_._2 >= 2)
  }

  def solutionToSecondHalf(lines: List[Line]): Int = {
    lines
      .flatMap(_.allPoints)
      .groupBy(identity)
      .map { case (p, points) => p -> points.size }
      .count(_._2 >= 2)
  }

  val inputLines: List[Line] =
    readAllLines("day-5-input.txt").map(_.strip).map(Line.fromString(_).get)

  println(solutionToFirstHalf(inputLines))
  println(solutionToSecondHalf(inputLines))
}
