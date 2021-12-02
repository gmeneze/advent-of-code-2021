import scala.util.Using
import scala.io.Source

object Day1SonarSweep extends App {
  Using(Source.fromFile("./inputs/day-1-input.txt")) { source =>
    val allDepths: List[Int] = source.getLines.map(_.toInt).toList

    def loop(depths: List[Int], count: Int): Int =
      depths match {
        // Terminal case
        case Nil => count
        // The depth increased, increase counter
        case x :: y :: zs if x < y => loop(y :: zs, count + 1)
        // The depth did not increase or its the last entry in the input
        case x :: ys => loop(ys, count)
      }

    println(loop(allDepths, 0))
  }
}
