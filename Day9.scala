import InputReader._
import scala.collection.mutable

object Day9 extends App {
  type Point = (Int, Int)

  def getLowPoints(matrix: Vector[Vector[Int]]): List[Point] = {
    val rowSize: Int = matrix.size
    val colSize: Int = matrix(0).size

    var lowPoints = Vector.empty[Point]
    for {
      r <- 0 until rowSize
      c <- 0 until colSize
    } yield {
      val num = matrix(r)(c)
      val up = if (r > 0) matrix(r - 1)(c) else Int.MaxValue
      val down = if (r < (rowSize - 1)) matrix(r + 1)(c) else Int.MaxValue
      val left = if (c > 0) matrix(r)(c - 1) else Int.MaxValue
      val right = if (c < (colSize - 1)) matrix(r)(c + 1) else Int.MaxValue

      if ((num < up) && (num < down) && (num < left) && (num < right))
        lowPoints = lowPoints.appended((r, c))
    }

    lowPoints.toList
  }

  def solutionToFirstHalf(matrix: Vector[Vector[Int]]): Int = {
    val rowSize: Int = matrix.size
    val colSize: Int = matrix(0).size

    val lowPoints: List[Point] = getLowPoints(matrix)

    lowPoints.foldLeft(0) { (acc, point) =>
      val (row, col) = point
      acc + matrix(row)(col) + 1
    }
  }

  def solutionToSecondHalf(matrix: Vector[Vector[Int]]): Int = {
    val rowSize: Int = matrix.size
    val colSize: Int = matrix(0).size

    // BFS
    def getBasinSize(point: Point): Int = {
      val queue = mutable.Queue.empty[Point]
      val seenPoints = mutable.Set.empty[Point]
      seenPoints.addOne(point)
      queue.enqueue(point)

      def getNeighbors(point: Point): List[Point] = {
        var neighbors = Vector.empty[Point]
        val (r, c) = point
        if (r > 0) neighbors = neighbors.appended((r - 1, c))
        if (r < (rowSize - 1)) neighbors = neighbors.appended((r + 1, c))
        if (c > 0) neighbors = neighbors.appended((r, c - 1))
        if (c < (colSize - 1)) neighbors = neighbors.appended((r, c + 1))
        neighbors.toList
      }

      while (!queue.isEmpty) {
        val currPoint = queue.dequeue()
        val neighbors = getNeighbors(currPoint)
        def isNotNine(point: Point): Boolean = {
          val (r, c) = point
          matrix(r)(c) != 9
        }
        neighbors.foreach { neighbor =>
          if (!seenPoints.contains(neighbor) && isNotNine(neighbor)) {
            seenPoints.addOne(neighbor)
            queue.enqueue(neighbor)
          }
        }
      }

      seenPoints.size
    }

    val lowPoints: List[Point] = getLowPoints(matrix)

    val largestBasinSizes: List[Int] =
      lowPoints.map(getBasinSize(_)).sorted.reverse.take(3)

    largestBasinSizes.reduce(_ * _)
  }

  val matrix: Vector[Vector[Int]] =
    readAllLines("day-9-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.split("").map(_.toInt).toVector)
      .toVector

  println(solutionToFirstHalf(matrix))
  println(solutionToSecondHalf(matrix))
}
