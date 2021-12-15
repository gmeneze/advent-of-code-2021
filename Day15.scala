import InputReader._

import scala.collection.mutable
object Day15 extends App {
  case class Position(row: Int, col: Int)
  type Grid = Vector[Vector[Int]]

  def solutionToFirstHalf(grid: Grid): Long = {
    val MAX_ROW = grid.size - 1
    val MAX_COL = grid(0).size - 1
    val TOTAL_ELEMS = grid.size * grid(0).size

    def getNeighbors(position: Position): List[Position] = {
      val Position(row, col) = position
      var neighborPositions = List.empty[Position]

      if (row + 1 <= MAX_ROW)
        neighborPositions = neighborPositions.appended(Position(row + 1, col))
      if (col + 1 <= MAX_COL)
        neighborPositions = neighborPositions.appended(Position(row, col + 1))
      if (row - 1 >= 0)
        neighborPositions = neighborPositions.appended(Position(row - 1, col))
      if (col - 1 >= 0)
        neighborPositions = neighborPositions.appended(Position(row, col - 1))

      neighborPositions
    }

    def dijkstrasSP: Long = {
      val risks = mutable.Map.empty[Position, Long]
      val visited = mutable.Set.empty[Position]

      case class RiskAndPos(risk: Long, pos: Position)
      val minHeap = mutable.PriorityQueue.empty[RiskAndPos](
        Ordering.fromLessThan(_.risk > _.risk)
      )

      minHeap.addOne(RiskAndPos(0, Position(0, 0)))

      while (!minHeap.isEmpty && visited.size != TOTAL_ELEMS) {
        while (visited.contains(minHeap.head.pos)) minHeap.dequeue
        val RiskAndPos(risk, pos) = minHeap.dequeue
        visited.addOne(pos)
        getNeighbors(pos).foreach { neighbor =>
          if (!visited.contains(neighbor)) {
            val nRisk = risk + grid(neighbor.row)(neighbor.col)
            val nCurrentMinRisk = risks.getOrElse(neighbor, Long.MaxValue)

            risks(neighbor) = math.min(nRisk, nCurrentMinRisk)
            minHeap.addOne(RiskAndPos(risks(neighbor), neighbor))
          }
        }
      }

      risks(Position(MAX_ROW, MAX_COL))
    }

    dijkstrasSP
  }

  def solutionToSecondHalf(grid: Grid): Long = {
    val ROW_SIZE = grid.size
    val COL_SIZE = grid(0).size

    val REAL_MAX_ROW = grid.size * 5 - 1
    val REAL_MAX_COL = grid(0).size * 5 - 1
    val TOTAL_ELEMS = (REAL_MAX_ROW + 1) * (REAL_MAX_COL + 1)

    def getNeighbors(position: Position): List[Position] = {
      val Position(row, col) = position
      var neighborPositions = List.empty[Position]

      if (row + 1 <= REAL_MAX_ROW)
        neighborPositions = neighborPositions.appended(Position(row + 1, col))
      if (col + 1 <= REAL_MAX_COL)
        neighborPositions = neighborPositions.appended(Position(row, col + 1))
      if (row - 1 >= 0)
        neighborPositions = neighborPositions.appended(Position(row - 1, col))
      if (col - 1 >= 0)
        neighborPositions = neighborPositions.appended(Position(row, col - 1))
      neighborPositions
    }

    val positionToLowestRisk = mutable.Map.empty[Position, Long]
    val destination = Position(REAL_MAX_ROW, REAL_MAX_COL)

    def getElement(pos: Position): Long = {
      val Position(row, col) = pos
      val rowAdder: Int = row / ROW_SIZE
      val colAdder: Int = col / COL_SIZE

      val actualRow = row % ROW_SIZE
      val actualCol = col % COL_SIZE

      val total = grid(actualRow)(actualCol) + rowAdder + colAdder
      if (total % 9L == 0L) 9L
      else (total % 9L)
    }

    def dijkstrasSP: Long = {
      val risks = mutable.Map.empty[Position, Long]
      val visited = mutable.Set.empty[Position]

      case class RiskAndPos(risk: Long, pos: Position)
      val minHeap = mutable.PriorityQueue.empty[RiskAndPos](
        Ordering.fromLessThan(_.risk > _.risk)
      )

      minHeap.addOne(RiskAndPos(0, Position(0, 0)))

      while (!minHeap.isEmpty && visited.size != TOTAL_ELEMS) {
        while (visited.contains(minHeap.head.pos)) minHeap.dequeue
        val RiskAndPos(risk, pos) = minHeap.dequeue
        visited.addOne(pos)
        getNeighbors(pos).foreach { neighbor =>
          if (!visited.contains(neighbor)) {
            val nRisk = risk + getElement(neighbor)
            val nCurrentMinRisk = risks.getOrElse(neighbor, Long.MaxValue)

            risks(neighbor) = math.min(nRisk, nCurrentMinRisk)
            minHeap.addOne(RiskAndPos(risks(neighbor), neighbor))
          }
        }
      }

      risks(Position(REAL_MAX_ROW, REAL_MAX_COL))
    }

    dijkstrasSP
  }

  val grid: Grid =
    readAllLines("day-15-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.split("").map(_.toInt).toVector)
      .toVector

  println(solutionToFirstHalf(grid))
  println(solutionToSecondHalf(grid))
}
