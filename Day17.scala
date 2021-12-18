import InputReader._

object Day17 extends App {
  case class TargetArea(xMin: Int, xMax: Int, yMin: Int, yMax: Int)

  def solutionToSecondHalf(targetArea: TargetArea): Long = {
    def hitsTarget(x: Int, y: Int, xVel: Int, yVel: Int): Boolean = {
      // println(s"x: $x, y:$y")
      val TargetArea(xMin, xMax, yMin, yMax) = targetArea

      (x, y) match {
        case (x, y) if (x > xMax || y < yMin) => false
        case (x, y) if (x >= xMin && x <= xMax && y >= yMin && y <= yMax) =>
          true
        case _ =>
          val xDrag: Int =
            if (xVel < 0) 1
            else if (xVel > 0) -1
            else 0

          val yDrag: Int = -1
          hitsTarget(x + xVel, y + yVel, xVel + xDrag, yVel + yDrag)
      }
    }

    val velocityValues: IndexedSeq[(Int, Int)] = for {
      xVel <- 0 to (targetArea.xMax)
      yVel <- targetArea.yMin to (-targetArea.yMin)
      if hitsTarget(0, 0, xVel, yVel)
    } yield (xVel, yVel)

    velocityValues.size
  }

  val inputTargetArea: TargetArea = {
    val pattern = """target area: x=(\d+)..(\d+), y=-(\d+)..-(\d+)""".r

    val inputString = readAll("day-17-input.txt").trim
    inputString match {
      case pattern(xMin, xMax, yMin, yMax) =>
        TargetArea(xMin.toInt, xMax.toInt, -yMin.toInt, -yMax.toInt)
    }
  }

  println(solutionToSecondHalf(inputTargetArea))
}
