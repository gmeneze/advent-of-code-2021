import InputReader._
import scala.collection.mutable
object Day14 extends App {
  case class Pair(first: Char, second: Char)
  def processStep(
      charsToCount: Map[Pair, Long],
      rules: Map[String, Char]
  ): Map[Pair, Long] = {
    val pairCount = mutable.Map.empty[Pair, Long]
    charsToCount.toList.foreach { case (Pair(first, second), occurence) =>
      val string = "" + first + second
      if (rules.contains(string)) {
        val middleChar = rules(string)
        pairCount(Pair(first, middleChar)) =
          pairCount.getOrElse(Pair(first, middleChar), 0L) + occurence
        pairCount(Pair(middleChar, second)) =
          pairCount.getOrElse(Pair(middleChar, second), 0L) + occurence
      } else {
        pairCount(Pair(first, second)) =
          pairCount.getOrElse(Pair(first, second), 0L) + occurence
      }
    }
    pairCount.toMap
  }

  def solution(
      input: String,
      rules: Map[String, Char],
      iterations: Int
  ): Long = {
    var pairCount: Map[Pair, Long] =
      input.toList
        .sliding(2, 1)
        .map { case List(first, second) => Pair(first, second) }
        .toList
        .groupBy(identity)
        .map { case (pair, occurences) => pair -> occurences.size.toLong }

    (1 to iterations).foreach { count =>
      pairCount = processStep(pairCount, rules)
    }

    var charToCount = mutable.Map.empty[Char, Long]

    charToCount(input.head) = 1L

    pairCount.foreach { case (Pair(_, second), count) =>
      charToCount(second) = charToCount.getOrElse(second, 0L) + count
    }

    val maxOccurence = charToCount.values.max
    val minOccurence = charToCount.values.min

    maxOccurence - minOccurence
  }

  val (input: String, rules: Map[String, Char]) = {
    def processRuleInput(line: String): (String, Char) = {
      val Array(str, char) = line.split(" -> ")
      str -> char.head
    }

    val entireInput =
      readAllLines("day-14-input.txt").map(_.trim).filterNot(_.isEmpty)
    val input = entireInput.head
    val rules = entireInput.tail.map(processRuleInput).toMap
    (input, rules)
  }

  // solution to first half
  println(solution(input, rules, 10))
  // solution to second half
  println(solution(input, rules, 40))
}
