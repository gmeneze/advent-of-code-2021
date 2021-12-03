import InputReader._

object Day1 extends App {
  def solutionToFirstHalf(allDepths: List[Int]): Int = {
    val slidingWindowOfTwoElems: Iterator[List[Int]] = allDepths.sliding(2, 1)
    slidingWindowOfTwoElems.foldLeft(0) {
      case (acc, List(firstDepth, secondDepth)) =>
        (secondDepth > firstDepth) match {
          case true  => acc + 1
          case false => acc
        }
    }
  }

  def solutionToSecondHalf(allDepths: List[Int]): Int = {
    val sumOfSlidingWindows: List[Int] =
      allDepths.sliding(3, 1).map(_.sum).toList
    solutionToFirstHalf(sumOfSlidingWindows)
  }

  val allDepths: List[Int] = readAllLines("day-1-input.txt")
    .map(_.trim)
    .filterNot(_.isEmpty)
    .map(_.toInt)

  println(solutionToFirstHalf(allDepths))

  println(solutionToSecondHalf(allDepths))
}
