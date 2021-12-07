import InputReader._

object Day7 extends App {
  def solutionToFirstHalf(crabPositions: List[Int]): Int = {
    val maxPos = crabPositions.max
    val minPos = crabPositions.min

    val fuelCosts = (minPos to maxPos).map { selectedPos =>
      crabPositions.foldLeft(0) { case (acc, curPos) =>
        acc + math.abs(selectedPos - curPos)
      }
    }

    fuelCosts.min
  }

  def solutionToSecondHalf(crabPositions: List[Int]): Int = {
    def fuelCostForSteps(stepCount: Int): Int = {
      // answer is = 1 + 2 + 3 ... stepCount
      // sum of natural nos. from 1 to n = n (n + 1)/2
      // reasoning: https://www.quora.com/How-do-I-find-the-sum-of-all-natural-numbers-from-1-to-n
      stepCount * (stepCount + 1) / 2
    }

    val maxPos = crabPositions.max
    val minPos = crabPositions.min

    val fuelCosts = (minPos to maxPos).map { selectedPos =>
      crabPositions.foldLeft(0) { case (acc, curPos) =>
        val stepCount = math.abs(selectedPos - curPos)
        acc + fuelCostForSteps(stepCount)
      }
    }

    fuelCosts.min
  }

  val crabPositions: List[Int] =
    readAll("day-7-input.txt").strip
      .split(",")
      .filterNot(_.isEmpty)
      .map(_.toInt)
      .toList

  println(solutionToFirstHalf(crabPositions))
  println(solutionToSecondHalf(crabPositions))
}
