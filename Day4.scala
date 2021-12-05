import InputReader._
import scala.annotation.tailrec
import scala.collection.mutable

object Day4 extends App {
  type Board = Vector[Vector[Int]]

  // 5 lines containing numbers will be provided, turn it into a board
  // Sample input:
  //   83 40 67 98  4
  //   50 74 31 30  3
  //   75 64 79 61  5
  //   12 59 26 25 72
  //   36 33 18 54 10
  def createBoard(lines: List[String]): Board = {
    lines.map { line =>
      val cleanLine = line.strip
      val row = cleanLine.split("\\s+").map(_.toInt).toVector
      row
    }.toVector
  }

  def printBoard(board: Board): Unit = {
    board.map { row =>
      println(row.toList)
    }
  }

  def calculateBoardSum(board: Board, seenNumbers: Set[Int]): Long =
    board.map { row =>
      row.filterNot(seenNumbers.contains(_)).sum
    }.sum

  // Process a board with a drawn number:
  //   - if the number exists on the board, check if the entire row OR column of the number is seen.
  //       - if yes, the board just won, return true
  //       - if no, return false
  def didBoardWin(
      board: Board,
      drawnNumber: Int,
      seenNumbers: Set[Int]
  ): Boolean = {
    type RowNum = Int
    type ColNum = Int

    @tailrec
    def searchBoard(row: RowNum, col: ColNum): Option[(RowNum, ColNum)] = {
      (row, col) match {
        case (5, _) => None
        case (_, 5) => searchBoard(row + 1, 0)
        case _ =>
          if (board(row)(col) == drawnNumber) Some((row, col))
          else searchBoard(row, col + 1)
      }
    }

    def checkIfBoardWon(row: RowNum, col: ColNum): Boolean = {
      val rowWon = (0 to 4).forall(i => seenNumbers.contains(board(row)(i)))
      val colWon = (0 to 4).forall(i => seenNumbers.contains(board(i)(col)))
      rowWon || colWon
    }

    searchBoard(0, 0) match {
      case None => false
      case Some((row, col)) =>
        checkIfBoardWon(row, col)
    }
  }

  def solutionToFirstHalf(
      boards: List[Board],
      drawnNumbers: List[Int]
  ): Long = {
    // find winning board
    // Assumptions:
    //   - Board will not contain negative numbers, we use -1 to mark drawn numbers.
    //   - Board will not contain the same number twice.
    //   - Multiple boards might win for a given drawn number,
    //     just returning the first winning board is enough for our purposes

    val seenNumbers = mutable.Set.empty[Int]
    var winningBoard: Option[Board] = Option.empty[Board]

    def findWinningBoard(num: Int): Option[Board] = {
      seenNumbers.addOne(num)
      winningBoard = boards.collectFirst {
        case board if didBoardWin(board, num, seenNumbers.toSet) => board
      }
      winningBoard
    }

    drawnNumbers.collectFirst {
      case num if findWinningBoard(num).isDefined =>
        calculateBoardSum(winningBoard.get, seenNumbers.toSet) * num
    }.get

    // loop(boards, drawnNumbers, Set.empty[Int])
  }

  def solutionToSecondHalf(
      boards: List[Board],
      drawnNumbers: List[Int]
  ): Long = {
    @tailrec
    def loop(
        boards: List[Board],
        drawnNumbers: List[Int],
        seenNumbers: Set[Int]
    ): Long =
      drawnNumbers match {
        case num :: nums =>
          val newSeenNumbers = seenNumbers + num
          val (winningBoards, losingBoards) =
            boards.partition(board => didBoardWin(board, num, newSeenNumbers))
          if (losingBoards.isEmpty)
            calculateBoardSum(winningBoards.head, newSeenNumbers) * num
          else loop(losingBoards, nums, newSeenNumbers)

        // this indicates something is wrong with the input
        case Nil => throw new AssertionError("this shouldn't have happened")
      }

    loop(boards, drawnNumbers, Set.empty[Int])
  }

  val inputLines = {
    readAllLines("day-4-input.txt").map(_.trim).filterNot(_.isEmpty)
  }

  val drawnNumbers: List[Int] =
    inputLines.head.trim.split(",").map(_.toInt).toList

  val boards: List[Board] = inputLines.tail.grouped(5).map(createBoard).toList

  println(solutionToFirstHalf(boards, drawnNumbers))

  println(solutionToSecondHalf(boards, drawnNumbers))
}
