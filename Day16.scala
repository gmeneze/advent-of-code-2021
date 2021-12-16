import InputReader._

import scala.util.{Try, Success, Failure}
object Day16 extends App {
  def binaryToDecimal(bits: List[Int]): Long = {
    bits.reverse.zipWithIndex.foldLeft(0L) { case (acc, (bit, idx)) =>
      acc + (bit.toLong << idx)
    }
  }

  case class Result(remainingBits: List[Int], value: Long)
  def processLiteralValue(bits: List[Int]): Result = {
    case class InternalResult(remainingBits: List[Int], valueBits: List[Int])
    def loop(bits: List[Int], valueBits: List[Int]): InternalResult = {
      bits match {
        case 1 :: a :: b :: c :: d :: xs =>
          loop(xs, valueBits ++ List(a, b, c, d))
        case 0 :: a :: b :: c :: d :: xs =>
          InternalResult(xs, valueBits ++ List(a, b, c, d))
      }
    }
    val InternalResult(remainingBits, valueBits) = loop(bits, List.empty[Int])
    val literalValue = binaryToDecimal(valueBits)
    Result(remainingBits, literalValue)
  }

  var versionAcc: List[Long] = List.empty[Long]

  def processPacket(bits: List[Int]): Try[Result] = Try {
    bits match {
      case v1 :: v2 :: v3 :: t1 :: t2 :: t3 :: restOfBits =>
        versionAcc = versionAcc.appended(binaryToDecimal(List(v1, v2, v3)))
        binaryToDecimal(List(t1, t2, t3)) match {
          case 4 => processLiteralValue(restOfBits)
          case typeId =>
            val lengthTypeId = restOfBits.head
            var remainingBits = restOfBits.tail
            var subpacketValues = List.empty[Long]
            lengthTypeId match {
              case 0 =>
                val subPacketBitCount = binaryToDecimal(remainingBits.take(15))
                remainingBits = remainingBits.drop(15)
                val currentBitCount = remainingBits.size
                while (
                  currentBitCount - remainingBits.size < subPacketBitCount
                ) {
                  val result = processPacket(remainingBits).get
                  remainingBits = result.remainingBits
                  subpacketValues = subpacketValues.appended(result.value)
                }
              case 1 =>
                val subPacketCount = binaryToDecimal(remainingBits.take(11))
                remainingBits = remainingBits.drop(11)
                for (_ <- 1L to subPacketCount) {
                  val result = processPacket(remainingBits).get
                  remainingBits = result.remainingBits
                  subpacketValues = subpacketValues.appended(result.value)
                }
            }
            val value: Long =
              typeId match {
                case 0 => subpacketValues.sum
                case 1 => subpacketValues.reduce(_ * _)
                case 2 => subpacketValues.min
                case 3 => subpacketValues.max
                case 5 =>
                  val List(first, second) = subpacketValues.take(2)
                  if (first > second) 1L
                  else 0L
                case 6 =>
                  val List(first, second) = subpacketValues.take(2)
                  if (first < second) 1L
                  else 0L
                case 7 =>
                  val List(first, second) = subpacketValues.take(2)
                  if (first == second) 1L
                  else 0L
                case _ =>
                  throw new IllegalArgumentException(s"invalid type-id $typeId")
              }
            Result(remainingBits, value)

        }
      case _ =>
        throw new IllegalArgumentException(
          s"bits: $bits does not have any packets"
        )
    }
  }

  def solutionToFirstHalf(bits: List[Int]): Long = {
    processPacket(bits)
    versionAcc.sum
  }

  def solutionToSecondHalf(bits: List[Int]): Long = {
    processPacket(bits).get.value
  }

  val hexToBinary =
    Map(
      '0' -> List(0, 0, 0, 0),
      '1' -> List(0, 0, 0, 1),
      '2' -> List(0, 0, 1, 0),
      '3' -> List(0, 0, 1, 1),
      '4' -> List(0, 1, 0, 0),
      '5' -> List(0, 1, 0, 1),
      '6' -> List(0, 1, 1, 0),
      '7' -> List(0, 1, 1, 1),
      '8' -> List(1, 0, 0, 0),
      '9' -> List(1, 0, 0, 1),
      'A' -> List(1, 0, 1, 0),
      'B' -> List(1, 0, 1, 1),
      'C' -> List(1, 1, 0, 0),
      'D' -> List(1, 1, 0, 1),
      'E' -> List(1, 1, 1, 0),
      'F' -> List(1, 1, 1, 1)
    )

  val inputBits: List[Int] =
    readAll("day-16-input.txt").trim.toList.flatMap(char => hexToBinary(char))

  println(solutionToFirstHalf(inputBits))
  println(solutionToSecondHalf(inputBits))

}
