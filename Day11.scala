import InputReader._

object Day11 extends App {
  type Octopus = (Int, Int)
  type Energy = Int
  type Line = Vector[Energy]
  type Grid = Vector[Line]

  final val ROW_SIZE = inputGrid.size
  final val COL_SIZE = inputGrid.head.size

  def processStep(grid: Grid): Grid = {
    def increaseEnergyByOne(grid: Grid): Grid = {
      grid.map(line => line.map(_ + 1))
    }

    def findFlashingOctopuses(grid: Grid): IndexedSeq[Octopus] = {
      val rowSize = grid.size
      val colSize = grid.head.size

      for {
        r <- 0 until ROW_SIZE
        c <- 0 until COL_SIZE
        if grid(r)(c) > 9
      } yield (r, c)
    }

    def processFlashingOctopus(grid: Grid, octopus: Octopus): Grid = {
      def getNeighbors(octopus: Octopus): IndexedSeq[Octopus] = {
        val (r, c) = octopus
        def isRowValid(row: Int) = (row >= 0) && (row < ROW_SIZE)
        def isColValid(col: Int) = (col >= 0) && (col < COL_SIZE)

        for {
          row <- (r - 1) to (r + 1)
          col <- (c - 1) to (c + 1)
          if (row, col) != (r, c)
          if isRowValid(row)
          if isColValid(col)
        } yield (row, col)
      }

      def processNeighbor(grid: Grid, octopus: Octopus): Grid = {
        val (r, c) = octopus
        val currentEnergy = grid(r)(c)
        currentEnergy match {
          case 0 =>
            grid // do nothing - the octopus has already flashed in this step
          case _ =>
            val updatedRow = grid(r).updated(c, currentEnergy + 1)
            grid.updated(r, updatedRow)
        }
      }

      def processOctopus(grid: Grid, octopus: Octopus): Grid = {
        val (r, c) = octopus
        val updatedRow = grid(r).updated(c, 0)
        grid.updated(r, updatedRow)
      }

      var currentGrid = processOctopus(grid, octopus)
      getNeighbors(octopus).foreach { octopus =>
        currentGrid = processNeighbor(currentGrid, octopus)
      }
      currentGrid
    }

    var currentGrid = increaseEnergyByOne(grid)
    var currentFlashingOctopuses = findFlashingOctopuses(currentGrid)

    while (!currentFlashingOctopuses.isEmpty) {
      currentFlashingOctopuses.foreach { octopus =>
        currentGrid = processFlashingOctopus(currentGrid, octopus)
      }
      currentFlashingOctopuses = findFlashingOctopuses(currentGrid)
    }

    currentGrid
  }

  def solutionToFirstHalf(grid: Grid): Long = {
    def flashedOctopuses(grid: Grid): Long = {
      grid.map(line => line.count(_ == 0)).sum
    }

    var currentGrid = grid
    (1 to 100).map { _ =>
      currentGrid = processStep(currentGrid)
      flashedOctopuses(currentGrid)
    }.sum
  }

  def solutionToSecondHalf(grid: Grid): Option[Int] = {
    def flashedOctopuses(grid: Grid): Long = {
      grid.map(line => line.count(_ == 0)).sum
    }

    val gridSize = ROW_SIZE * COL_SIZE

    var currentGrid = grid

    (1 to 1000).foreach { step =>
      currentGrid = processStep(currentGrid)
      if (flashedOctopuses(currentGrid) == gridSize) return Some(step)
    }
    None
  }

  lazy val inputGrid: Grid =
    readAllLines("day-11-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(line => line.split("").map(_.toInt).toVector)
      .toVector

  println(solutionToFirstHalf(inputGrid))
  println(solutionToSecondHalf(inputGrid).get)
}
