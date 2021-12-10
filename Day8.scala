import InputReader._
import scala.collection.mutable

object Day8 extends App {
  case class Digit(pattern: Set[Char]) {
    def isOne = pattern.size == 2
    def isFour = pattern.size == 4
    def isSeven = pattern.size == 3
    def isEight = pattern.size == 7
    // Any other digit can be verified using 1, 4, 7, 8
    def isZero(one: Digit, four: Digit) = {
      val oneIntersection = pattern intersect one.pattern
      val fourIntersection = pattern intersect four.pattern
      (pattern.size == 6) && (oneIntersection.size == 2) && (fourIntersection.size == 3)
    }
    def isSix(one: Digit, four: Digit) = {
      val oneIntersection = pattern intersect one.pattern
      val fourIntersection = pattern intersect four.pattern
      (pattern.size == 6) && (oneIntersection.size == 1) && (fourIntersection.size == 3)
    }
    def isNine(one: Digit, four: Digit) = {
      val oneIntersection = pattern intersect one.pattern
      val fourIntersection = pattern intersect four.pattern
      (pattern.size == 6) && (oneIntersection.size == 2) && (fourIntersection.size == 4)
    }
    def isFive(four: Digit, seven: Digit) = {
      val fourIntersection = pattern intersect four.pattern
      val sevenIntersection = pattern intersect seven.pattern
      (pattern.size == 5) && (fourIntersection.size == 3) && (sevenIntersection.size == 2)
    }
    def isTwo(four: Digit, seven: Digit) = {
      val fourIntersection = pattern intersect four.pattern
      val sevenIntersection = pattern intersect seven.pattern
      (pattern.size == 5) && (fourIntersection.size == 2) && (sevenIntersection.size == 2)
    }
    def isThree(four: Digit, seven: Digit) = {
      val fourIntersection = pattern intersect four.pattern
      val sevenIntersection = pattern intersect seven.pattern
      (pattern.size == 5) && (fourIntersection.size == 3) && (sevenIntersection.size == 3)
    }
  }

  object Digit {
    def fromString(s: String): Digit = Digit(s.trim.toSet)
  }

  def solutionToFirstHalf(input: List[String]) = {
    val digits = input
      .map(_.split("\\|").last)
      .flatMap(_.split(" "))
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(Digit.fromString(_))

    digits.filter(d => d.isOne || d.isFour || d.isSeven || d.isEight).size
  }

  def solutionToSecondHalf(input: List[String]) = {
    def parsePatterns(digits: List[Digit]): Map[Digit, Int] = {
      val numToDigit = mutable.Map.empty[Int, Digit]

      // first pass: find 1, 4, 7, 8
      digits.foreach {
        case d if d.isOne   => numToDigit.addOne(1 -> d)
        case d if d.isFour  => numToDigit.addOne(4 -> d)
        case d if d.isSeven => numToDigit.addOne(7 -> d)
        case d if d.isEight => numToDigit.addOne(8 -> d)
        case _ => // allow other digits to fall through in this pass
      }

      // second pass: find all other digits (using 1, 4, 7, 8)
      digits.foreach {
        case d if d.isZero(numToDigit(1), numToDigit(4)) =>
          numToDigit.addOne(0 -> d)
        case d if d.isSix(numToDigit(1), numToDigit(4)) =>
          numToDigit.addOne(6 -> d)
        case d if d.isNine(numToDigit(1), numToDigit(4)) =>
          numToDigit.addOne(9 -> d)
        case d if d.isFive(numToDigit(4), numToDigit(7)) =>
          numToDigit.addOne(5 -> d)
        case d if d.isTwo(numToDigit(4), numToDigit(7)) =>
          numToDigit.addOne(2 -> d)
        case d if d.isThree(numToDigit(4), numToDigit(7)) =>
          numToDigit.addOne(3 -> d)
        case _ => // allow 1, 4, 7, 8 to fall through in this pass
      }

      numToDigit.map { case (k, v) => v -> k }.toMap
    }

    def getNumber(
        outputValues: List[Digit],
        digitToNum: Map[Digit, Int]
    ): Int = {
      outputValues.foldLeft(0) { (acc, x) =>
        val xNum: Int = digitToNum(x)
        acc * 10 + xNum
      }
    }

    input.foldLeft(0) { case (acc, string) =>
      val Array(patternsStr: String, outputValueStr: String) =
        string.split("\\|")

      val patterns: List[Digit] =
        patternsStr
          .split(" ")
          .map(_.trim)
          .filterNot(_.isEmpty)
          .map(Digit.fromString(_))
          .toList

      val outputValues: List[Digit] =
        outputValueStr
          .split(" ")
          .map(_.trim)
          .filterNot(_.isEmpty)
          .map(Digit.fromString(_))
          .toList

      val digitToNum: Map[Digit, Int] = parsePatterns(patterns)

      acc + getNumber(outputValues, digitToNum)
    }
  }

  val input: List[String] =
    readAllLines("day-8-input.txt").map(_.trim).filterNot(_.isEmpty)

  println(solutionToFirstHalf(input))
  println(solutionToSecondHalf(input))
}
