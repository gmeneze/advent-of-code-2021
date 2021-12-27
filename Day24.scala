import InputReader._

import scala.util.matching.Regex
import scala.util.{Try, Success, Failure}
import scala.collection.mutable

object Day24 extends App {
  sealed trait Operand

  case class Number(value: Int) extends Operand {
    override def toString: String = value.toString
  }

  object Number {
    def fromString(str: String): Try[Number] = Try {
      val num: Int =
        if (str.head == '-') -1 * str.tail.toInt
        else str.toInt

      Number(num)
    }
  }

  sealed trait Variable extends Operand
  case object W extends Variable
  case object X extends Variable
  case object Y extends Variable
  case object Z extends Variable

  object Variable {
    def fromString(str: String): Try[Variable] = Try {
      assert(str.size == 1, s"$str cannot be converted into Variable type")
      str.head match {
        case 'w' => W
        case 'x' => X
        case 'y' => Y
        case 'z' => Z
      }
    }
  }

  object Operand {
    def fromString(str: String): Operand = {
      val input = str.trim
      Number.fromString(input) match {
        case Failure(_)   => Variable.fromString(input).get
        case Success(num) => num
      }
    }
  }

  sealed trait Instruction
  case class Inp(op: Operand) extends Instruction {
    assert(op.isInstanceOf[Variable])
  }
  case class Add(op1: Operand, op2: Operand) extends Instruction {
    assert(op1.isInstanceOf[Variable])
  }
  case class Mul(op1: Operand, op2: Operand) extends Instruction {
    assert(op1.isInstanceOf[Variable])
  }
  case class Div(op1: Operand, op2: Operand) extends Instruction {
    assert(op1.isInstanceOf[Variable])
  }
  case class Mod(op1: Operand, op2: Operand) extends Instruction {
    assert(op1.isInstanceOf[Variable])
  }
  case class Eql(op1: Operand, op2: Operand) extends Instruction {
    assert(op1.isInstanceOf[Variable])
  }

  object Instruction {
    def fromString(str: String): Instruction = {
      val unaryPattern: Regex = """([a-z]+) ([wxyz])""".r
      val binaryPattern: Regex = """([a-z]+) ([wxyz]) (\S+)""".r

      str.trim match {
        case unaryPattern(op, first) =>
          op match {
            case "inp" => Inp(Operand.fromString(first))
          }
        case binaryPattern(op, first, second) =>
          op match {
            case "add" =>
              Add(Operand.fromString(first), Operand.fromString(second))
            case "mul" =>
              Mul(Operand.fromString(first), Operand.fromString(second))
            case "div" =>
              Div(Operand.fromString(first), Operand.fromString(second))
            case "mod" =>
              Mod(Operand.fromString(first), Operand.fromString(second))
            case "eql" =>
              Eql(Operand.fromString(first), Operand.fromString(second))
          }
      }
    }
  }

  sealed trait MonadStatus
  case object Valid extends MonadStatus
  case object Invalid extends MonadStatus

  def runMonadProgram(
      program: List[Instruction],
      input: Vector[Number]
  ): Option[Vector[Number]] = {
    val VARIABLE_VALUES = mutable.Map.empty[Variable, Int].withDefaultValue(0)
    var inputNumbers = input

    def getOperand(op: Operand): Int =
      op match {
        case x: Number   => x.value
        case x: Variable => VARIABLE_VALUES(x)
      }

    def setOperand(op: Operand, value: Operand): Unit = {
      assert(op.isInstanceOf[Variable])

      val variable = op.asInstanceOf[Variable]

      val num: Int =
        value match {
          case x: Number   => x.value
          case x: Variable => VARIABLE_VALUES(x)
        }
      VARIABLE_VALUES(variable) = num
    }

    var idx = 0
    var resultVector = Vector.empty[Number]
    for (inst <- program) {
      inst match {
        case Inp(op) =>
          val value: Int =
            idx match {
              // use input instruction heuristics: https://www.youtube.com/watch?v=Eswmo7Y7C4U&t=632s
              case 3 =>
                (getOperand(Z) % 26) - 6
              case 6 =>
                (getOperand(Z) % 26) - 9
              case 9 =>
                (getOperand(Z) % 26) - 5
              case 10 =>
                (getOperand(Z) % 26) - 9
              case 11 =>
                (getOperand(Z) % 26) - 5
              case 12 =>
                (getOperand(Z) % 26) - 2
              case 13 =>
                (getOperand(Z) % 26) - 7
              case _ =>
                val result = inputNumbers.head.value
                inputNumbers = inputNumbers.tail
                result
            }
          idx += 1
          if (value < 1 || value > 9) return None
          resultVector :+= Number(value)
          setOperand(op, Number(value))
        case Add(op1, op2) =>
          val first: Int = getOperand(op1)
          val second: Int = getOperand(op2)
          setOperand(op1, Number(first + second))
        case Mul(op1, op2) =>
          val first: Int = getOperand(op1)
          val second: Int = getOperand(op2)
          setOperand(op1, Number(first * second))
        case Div(op1, op2) =>
          val first: Int = getOperand(op1)
          val second: Int = getOperand(op2)
          setOperand(op1, Number(first / second))
        case Mod(op1, op2) =>
          val first: Int = getOperand(op1)
          val second: Int = getOperand(op2)
          setOperand(op1, Number(first % second))
        case Eql(op1, op2) =>
          val first: Int = getOperand(op1)
          val second: Int = getOperand(op2)
          val result = if (first == second) 1 else 0
          setOperand(op1, Number(result))
      }
    }

    (getOperand(Z) == 0) match {
      case true  => Some(resultVector)
      case false => None
    }
  }

  def solutionToFirstHalf(
      monadProgram: List[Instruction]
  ): Option[Vector[Number]] = {
    // try 9999...9 then 9999...8 and so on
    def loop(comb: Vector[Number]): Option[Vector[Number]] = {
      // use input instruction heuristics: https://www.youtube.com/watch?v=Eswmo7Y7C4U&t=632s
      if (comb.size == 7) {
        runMonadProgram(monadProgram, comb) match {
          case Some(numbers) =>
            println(
              s"tried combination: ${comb.mkString}, got result: ${numbers.mkString}, succeeded"
            )
            Some(numbers)
          case None =>
            println(s"tried combination: ${comb.mkString}, failed")
            None
        }
      } else {
        var num = 9
        var resultFound = false
        var result: Option[Vector[Number]] = None

        while (num != 0 && !resultFound) {
          val number = Number(num)
          loop(comb.appended(number)) match {
            case Some(res) =>
              resultFound = true
              result = Some(res)
            case None => // move on to next num
              num -= 1
          }
        }

        result
      }
    }

    loop(Vector.empty[Number])
  }

  def solutionToSecondHalf(
      monadProgram: List[Instruction]
  ): Option[Vector[Number]] = {
    // try 9999...9 then 9999...8 and so on
    def loop(comb: Vector[Number]): Option[Vector[Number]] = {
      // use input instruction heuristics: https://www.youtube.com/watch?v=Eswmo7Y7C4U&t=632s
      if (comb.size == 7) {
        runMonadProgram(monadProgram, comb) match {
          case Some(numbers) =>
            println(
              s"tried combination: ${comb.mkString}, got result: ${numbers.mkString}, succeeded"
            )
            Some(numbers)
          case None =>
            println(s"tried combination: ${comb.mkString}, failed")
            None
        }
      } else {
        var num = 1
        var resultFound = false
        var result: Option[Vector[Number]] = None

        while (num != 10 && !resultFound) {
          val number = Number(num)
          loop(comb.appended(number)) match {
            case Some(res) =>
              resultFound = true
              result = Some(res)
            case None => // move on to next num
              num += 1
          }
        }

        result
      }
    }

    loop(Vector.empty[Number])
  }

  val MONAD_PROGRAM: List[Instruction] =
    readAllLines("day-24-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(Instruction.fromString)
  println(solutionToFirstHalf(MONAD_PROGRAM))
  println(solutionToSecondHalf(MONAD_PROGRAM))
}
