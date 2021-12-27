import InputReader._

object Day25 extends App {
  case class Cucumber(row: Int, col: Int)

  case class State(
      eastCucumbers: Set[Cucumber],
      southCucumbers: Set[Cucumber],
      rowSize: Int,
      colSize: Int
  ) {
    def display: Unit = {
      for (i <- 0 until rowSize) {
        for (j <- 0 until colSize) {
          val cucumber = Cucumber(i, j)
          if (eastCucumbers contains cucumber) print(">")
          else if (southCucumbers contains cucumber) print("v")
          else print(".")
        }
        print("\n")
      }
    }

    def nextState: State = {
      var newEastCucumbers = Set.empty[Cucumber]
      for (cucumber <- eastCucumbers) {
        val nextCol = (cucumber.col + 1) % colSize
        val nextCucumber = cucumber.copy(col = nextCol)
        if (
          eastCucumbers
            .contains(nextCucumber) || southCucumbers.contains(nextCucumber)
        ) {
          newEastCucumbers += cucumber
        } else {
          newEastCucumbers += nextCucumber
        }
      }

      var newSouthCucumbers = Set.empty[Cucumber]
      for (cucumber <- southCucumbers) {
        val nextRow = (cucumber.row + 1) % rowSize
        val nextCucumber = cucumber.copy(row = nextRow)
        if (
          newEastCucumbers
            .contains(nextCucumber) || southCucumbers.contains(nextCucumber)
        ) {
          newSouthCucumbers += cucumber
        } else {
          newSouthCucumbers += nextCucumber
        }
      }

      State(newEastCucumbers, newSouthCucumbers, rowSize, colSize)
    }
  }

  def solutionToFirstHalf(state: State): Int = {
    var stepId = 0
    var currState: State = state
    var prevState: State = null
    while (currState != prevState) {
      println(s"step id: $stepId")
      currState.display
      println()
      stepId += 1
      val nextState: State = currState.nextState
      prevState = currState
      currState = nextState
    }

    stepId
  }

  val inputState: State = {
    val inputLines: List[String] =
      readAllLines("day-25-input.txt")
        .map(_.trim)
        .filterNot(_.isEmpty)

    var eastCucumbers = Set.empty[Cucumber]
    var southCucumbers = Set.empty[Cucumber]
    val rowSize: Int = inputLines.size
    val colSize: Int = inputLines.head.size

    for ((line, rowId) <- inputLines.zipWithIndex) {
      for ((char, colId) <- line.toList.zipWithIndex) {
        if (char == '>') eastCucumbers += Cucumber(rowId, colId)
        else if (char == 'v') southCucumbers += Cucumber(rowId, colId)
      }
    }

    State(eastCucumbers, southCucumbers, rowSize, colSize)
  }

  println(solutionToFirstHalf(inputState))
}
