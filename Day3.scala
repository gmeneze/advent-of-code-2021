import InputReader._

object Day3 extends App {
  type Binary = Vector[Int]

  def binaryToDecimal(binary: Binary): Long = {
    binary.reverse.zipWithIndex.foldLeft(0L) { case (acc, (bin, index)) =>
      bin match {
        case 0 => acc
        case 1 => acc + math.pow(2, index).toLong
      }
    }
  }

  def solutionToFirstHalf(binaryList: List[Binary]): Long = {
    val binaryGammaRate = {
      val totalOnes: Vector[Int] = binaryList.reduceLeft { (list1, list2) =>
        list1.zip(list2).map { case (x, y) => x + y }
      }

      totalOnes.map { ones =>
        val zeros = binaryList.size - ones
        (ones > zeros) match {
          case true  => 1
          case false => 0
        }
      }
    }

    val binaryEpsilonRate = binaryGammaRate.map(_ ^ 1)

    val gammaRate = binaryToDecimal(binaryGammaRate)
    val epsilonRate = binaryToDecimal(binaryEpsilonRate)

    gammaRate * epsilonRate
  }

  def solutionToSecondHalf(binaryList: List[Binary]): Long = {
    def getOnesInBitPosition(binaryList: List[Binary], position: Int): Int =
      binaryList.foldLeft(0)((acc, binary) => acc + binary(position))

    def getBinaryOxygenRating(binaryList: List[Binary], position: Int): Binary =
      binaryList match {
        // Terminal case
        case binary :: Nil => binary
        case _ =>
          val ones = getOnesInBitPosition(binaryList, position)
          val zeros = binaryList.size - ones
          (zeros > ones) match {
            case true =>
              val newBinaryList =
                binaryList.filter(binary => binary(position) == 0)
              getBinaryOxygenRating(newBinaryList, position + 1)
            case false =>
              val newBinaryList =
                binaryList.filter(binary => binary(position) == 1)
              getBinaryOxygenRating(newBinaryList, position + 1)
          }
      }

    def getBinaryCO2Rating(binaryList: List[Binary], position: Int): Binary =
      binaryList match {
        // Terminal case
        case binary :: Nil => binary
        case _ =>
          val ones = getOnesInBitPosition(binaryList, position)
          val zeros = binaryList.size - ones
          (zeros <= ones) match {
            case true =>
              val newBinaryList =
                binaryList.filter(binary => binary(position) == 0)
              getBinaryCO2Rating(newBinaryList, position + 1)
            case false =>
              val newBinaryList =
                binaryList.filter(binary => binary(position) == 1)
              getBinaryCO2Rating(newBinaryList, position + 1)
          }
      }

    val oxygenRating: Long = {
      val binaryOxygenRating: Binary = getBinaryOxygenRating(binaryList, 0)
      binaryToDecimal(binaryOxygenRating)
    }

    val co2Rating: Long = {
      val binaryCo2Rating: Binary = getBinaryCO2Rating(binaryList, 0)
      binaryToDecimal(binaryCo2Rating)
    }

    oxygenRating * co2Rating

  }

  val binaryList: List[Binary] = {
    val allLines =
      readAllLines("day-3-input.txt").map(_.trim).filterNot(_.isEmpty)

    val stringToBinary: String => Binary = s =>
      s.split("").map(_.toInt).toVector

    allLines.map(stringToBinary)
  }

  println(solutionToFirstHalf(binaryList))
  println(solutionToSecondHalf(binaryList))
}
