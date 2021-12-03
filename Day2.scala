import InputReader._
import scala.util.{Try, Success, Failure}

sealed trait Direction

object Direction {
  case object Forward extends Direction
  case object Down extends Direction
  case object Up extends Direction

  def fromString(str: String): Try[Direction] =
    str match {
      case "forward" => Success(Forward)
      case "down"    => Success(Down)
      case "up"      => Success(Up)
      case invalidDir =>
        Failure(
          new IllegalArgumentException(s"direction: $invalidDir not found")
        )
    }
}

case class Command(direction: Direction, moves: Int)

object Command {
  def fromString(str: String): Try[Command] =
    str.split(" ") match {
      case Array(dirStr: String, movesStr: String) =>
        for {
          direction <- Direction.fromString(dirStr)
          moves <- Try(movesStr.toInt)
        } yield Command(direction, moves)
      case _ =>
        Failure(
          new IllegalArgumentException(s"input: $str is not a valid command")
        )
    }
}

object Day2 extends App {
  def solutionToFirstHalf(allCommands: List[Command]): Int = {
    import Direction._
    val totalForwardMoves =
      allCommands.filter(_.direction == Forward).map(_.moves).sum
    val totalUpMoves = allCommands.filter(_.direction == Up).map(_.moves).sum
    val totalDownMoves =
      allCommands.filter(_.direction == Down).map(_.moves).sum

    val finalHorizontalPos = totalForwardMoves
    val finalDepth = (totalDownMoves - totalUpMoves)

    finalHorizontalPos * finalDepth
  }

  def solutionToSecondHalf(allCommands: List[Command]): Int = {
    import Direction._
    case class Tracker(horizontalPos: Int, depth: Int, aim: Int)

    val finalPos = allCommands.foldLeft(Tracker(0, 0, 0)) {
      case (
            Tracker(curHorizontalPos, curDepth, curAim),
            Command(direction, moves)
          ) =>
        direction match {
          case Down => Tracker(curHorizontalPos, curDepth, curAim + moves)
          case Up   => Tracker(curHorizontalPos, curDepth, curAim - moves)
          case Forward =>
            Tracker(
              curHorizontalPos + moves,
              curDepth + (curAim * moves),
              curAim
            )
        }
    }

    finalPos.horizontalPos * finalPos.depth
  }

  val allCommands: List[Command] = {
    val allLines =
      readAllLines("day-2-input.txt").map(_.trim).filterNot(_.isEmpty)
    allLines.map(Command.fromString(_).get)
  }

  println(solutionToFirstHalf(allCommands))
  println(solutionToSecondHalf(allCommands))
}
