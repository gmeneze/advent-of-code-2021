import scala.util.Using
import scala.io.Source

object Day1SonarSweep extends App {
  val allDepths: List[Int] =
    Using(Source.fromFile("./inputs/day-1-input.txt")) {
      _.getLines.map(_.strip.toInt).toList
    }.get

  def numTimesMeasurementIncreases: Int = {
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

  def numTimesMeasurementSlidingWindowIncreases: Int = {
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

  // solution to part 1
  println(numTimesMeasurementIncreases)

  // solution to part 2
  println(numTimesMeasurementSlidingWindowIncreases)
}
