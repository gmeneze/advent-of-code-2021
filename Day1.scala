import InputReader._

object Day1 extends App {
  def solutionToFirstHalf(allDepths: List[Int]): Int = {
    def loop(depths: List[Int], count: Int): Int =
      depths match {
        // Terminal case
        case Nil => count

        // The depth increased, increase counter
        case x :: y :: zs if x < y => loop(y :: zs, count + 1)

        // The depth did not increase or its the last entry in the input
        case x :: ys => loop(ys, count)
      }

    loop(allDepths, 0)
  }

  def solutionToSecondHalf(allDepths: List[Int]): Int = {
    def loop(depths: List[Int], count: Int): Int =
      depths match {
        // Atleast 4 elements need to exist for a comparison
        // two sliding widows are : (a, b, c) & (b, c, d)
        // the difference in sum between the second and first windows = d - a
        // if this difference is positive, increase counter
        case a :: b :: c :: d :: es if d > a =>
          loop(b :: c :: d :: es, count + 1)

        // if the difference is not positive, no change to counter
        case a :: b :: c :: d :: es => loop(b :: c :: d :: es, count)

        // Terminal case: There are less than 4 elements
        case _ => count
      }

    loop(allDepths, 0)
  }

  val allDepths: List[Int] = readAllLines("day-1-input.txt")
    .map(_.trim)
    .filterNot(_.isEmpty)
    .map(_.toInt)

  println(solutionToFirstHalf(allDepths))

  println(solutionToSecondHalf(allDepths))
}
