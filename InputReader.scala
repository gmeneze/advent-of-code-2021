import scala.util.Using
import scala.io.Source

object InputReader extends App {
  def readAllLines(fileName: String): List[String] =
    Using(Source.fromFile(s"./inputs/$fileName"))(_.getLines.toList).get

  def readAll(fileName: String): String =
    readAllLines(fileName).mkString("\n")
}
