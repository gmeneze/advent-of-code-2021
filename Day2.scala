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
  val allCommands: List[Command] = {
    val allLines =
      readAllLines("day-2-input.txt").map(_.trim).filterNot(_.isEmpty)
    allLines.map(Command.fromString(_).get)
  }

  import Direction._
  val totalForwardMoves =
    allCommands.filter(_.direction == Forward).map(_.moves).sum
  val totalUpMoves = allCommands.filter(_.direction == Up).map(_.moves).sum
  val totalDownMoves = allCommands.filter(_.direction == Down).map(_.moves).sum

  println(totalForwardMoves * (totalDownMoves - totalUpMoves))
}
