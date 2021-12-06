import InputReader._
import scala.collection.mutable

object Day6 extends App {
  val familySizeCache = mutable.Map.empty[(Int, Int), Long]

  case class Fish(daysToBirth: Int) {
    def familySize(afterDays: Int): Long = {
      if (familySizeCache.contains((daysToBirth, afterDays)))
        return familySizeCache((daysToBirth, afterDays))

      val result = (afterDays > daysToBirth) match {
        case true =>
          val leftDays = afterDays - (daysToBirth + 1)
          Fish(6).familySize(leftDays) + Fish(8).familySize(leftDays)
        case false =>
          1
      }

      familySizeCache((daysToBirth, afterDays)) = result
      result
    }
  }

  def solutionToFirstAndSecondHalf(fishes: Vector[Fish], afterDays: Int): Long = {
    fishes.map(_.familySize(afterDays)).sum
  }

  val inputState: Vector[Fish] =
    readAll("day-6-input.txt")
      .split(",")
      .map(_.strip)
      .filterNot(_.isEmpty)
      .map(s => Fish(s.toInt))
      .toVector

  println(solutionToFirstAndSecondHalf(inputState, 256))

}
