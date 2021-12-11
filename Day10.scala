import InputReader._
import scala.collection.mutable

object Day10 extends App {
  type Bracket = Char
  type Line = List[Bracket]

  final val LEGAL_PAIRS = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  def isOpeningBracket(bracket: Bracket): Boolean =
    LEGAL_PAIRS.contains(bracket)

  // check if line corrupted, if yes, return first illegal character
  def isLineCorrupted(line: Line): Option[Bracket] = {
    val stack = mutable.Stack.empty[Bracket]

    def islegalBracket(bracket: Bracket): Boolean = {
      isOpeningBracket(bracket) match {
        case true =>
          stack.push(bracket)
          true
        case false =>
          stack.isEmpty match {
            case false =>
              val openingBracket = stack.pop
              LEGAL_PAIRS(openingBracket) == bracket
            // if stack is empty there is no opening bracket found for this input closing bracket
            case true => false
          }
      }
    }

    line.collectFirst { case bracket if !islegalBracket(bracket) => bracket }
  }

  def solutionToFirstHalf(lines: List[Line]): Long = {
    val ILLEGAL_BRACKET_SCORES =
      Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

    lines.foldLeft(0L) { case (acc, line) =>
      isLineCorrupted(line) match {
        case Some(bracket) =>
          acc + ILLEGAL_BRACKET_SCORES(bracket)
        case None => acc + 0
      }
    }
  }

  def solutionToSecondHalf(lines: List[Line]): Long = {
    val CLOSING_BRACKET_SCORES = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

    def computeIncompleteLineScore(line: Line): Long = {
      val stack = mutable.Stack.empty[Bracket]

      line.foreach { bracket =>
        isOpeningBracket(bracket) match {
          case true  => stack.push(bracket)
          case false =>
            // only dealing with incomplete lines now, they are not corrupted
            stack.pop()
        }
      }

      var score = 0L
      while (!stack.isEmpty) {
        val openingBracket = stack.pop()
        val closingBracket = LEGAL_PAIRS(openingBracket)
        score = score * 5 + CLOSING_BRACKET_SCORES(closingBracket)
      }

      score
    }

    val incompleteLines: Vector[Line] =
      lines.filterNot(line => isLineCorrupted(line).isDefined).toVector

    val scores: Vector[Long] =
      incompleteLines.map(computeIncompleteLineScore).sorted

    val middleIndex: Int = scores.size / 2

    scores(middleIndex)
  }

  val inputLines: List[Line] =
    readAllLines("day-10-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toList)

  println(solutionToFirstHalf(inputLines))
  println(solutionToSecondHalf(inputLines))
}
