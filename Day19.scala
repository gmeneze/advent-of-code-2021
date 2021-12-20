import InputReader._

import scala.util.Try

object Day19 extends App {
  case class Coord(x: Int, y: Int, z: Int)

  // source: https://euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
  val ALL_TRANSFORMATIONS: List[Coord => Coord] =
    List(
      { case Coord(x, y, z) => Coord(x, y, z) },
      { case Coord(x, y, z) => Coord(x, z, -y) },
      { case Coord(x, y, z) => Coord(x, -y, -z) },
      { case Coord(x, y, z) => Coord(x, -z, y) },
      { case Coord(x, y, z) => Coord(y, -x, z) },
      { case Coord(x, y, z) => Coord(y, z, x) },
      { case Coord(x, y, z) => Coord(y, x, -z) },
      { case Coord(x, y, z) => Coord(y, -z, -x) },
      { case Coord(x, y, z) => Coord(-x, -y, z) },
      { case Coord(x, y, z) => Coord(-x, -z, -y) },
      { case Coord(x, y, z) => Coord(-x, y, -z) },
      { case Coord(x, y, z) => Coord(-x, z, y) },
      { case Coord(x, y, z) => Coord(-y, x, z) },
      { case Coord(x, y, z) => Coord(-y, -z, x) },
      { case Coord(x, y, z) => Coord(-y, -x, -z) },
      { case Coord(x, y, z) => Coord(-y, z, -x) },
      { case Coord(x, y, z) => Coord(z, y, -x) },
      { case Coord(x, y, z) => Coord(z, x, y) },
      { case Coord(x, y, z) => Coord(z, -y, x) },
      { case Coord(x, y, z) => Coord(z, -x, -y) },
      { case Coord(x, y, z) => Coord(-z, -y, -x) },
      { case Coord(x, y, z) => Coord(-z, -x, y) },
      { case Coord(x, y, z) => Coord(-z, y, x) },
      { case Coord(x, y, z) => Coord(-z, x, -y) }
    )

  case class Beacon(coord: Coord)

  object Beacon {
    def fromString(str: String): Try[Beacon] = Try {
      str.split(",").toList match {
        case List(xStr, yStr, zStr) =>
          Beacon(Coord(xStr.toInt, yStr.toInt, zStr.toInt))
        case _ =>
          throw new IllegalArgumentException(
            s"$str cannot be decoded into Beacon"
          )
      }
    }
  }

  case class Delta(x: Int, y: Int, z: Int)
  case class OverlapResult(delta: Delta, transformation: Coord => Coord)

  case class Scanner(beacons: Set[Beacon]) {
    def checkOverlap(that: Scanner): Option[OverlapResult] = {
      for {
        thisBeacon <- this.beacons
        transformation <- ALL_TRANSFORMATIONS
        transformedBeacons: Set[Beacon] =
          that.beacons.map(beacon => Beacon(transformation(beacon.coord)))
        thatBeacon <- transformedBeacons
      } {
        val delta = Delta(
          thatBeacon.coord.x - thisBeacon.coord.x,
          thatBeacon.coord.y - thisBeacon.coord.y,
          thatBeacon.coord.z - thisBeacon.coord.z
        )

        val thatBeaconsFromThisScanner = transformedBeacons.map { beacon =>
          val Coord(x, y, z) = beacon.coord
          Beacon(Coord(x - delta.x, y - delta.y, z - delta.z))
        }

        val thisBeacons = this.beacons

        val intersection: Set[Beacon] =
          thisBeacons intersect thatBeaconsFromThisScanner

        if (intersection.size >= 12)
          return Some(OverlapResult(delta, transformation))
      }

      None
    }
  }

  object Scanner {
    def fromString(str: String): Try[Scanner] = Try {
      val lines = str.split("\n").map(_.trim).toList
      require(lines.head.contains("scanner"))

      val beacons: Set[Beacon] =
        lines.tail.map(s => Beacon.fromString(s).get).toSet

      Scanner(beacons)
    }
  }

  var deltas: Set[Delta] = Set(Delta(0, 0, 0))

  def solutionToFirstHalf(scanners: List[Scanner]): Long = {
    var baseScanner: Scanner = scanners.head
    var remainingScanners: Set[Scanner] = scanners.tail.toSet

    while (!remainingScanners.isEmpty) {
      println(s"remaining scanner count: ${remainingScanners.size}")
      var scannersNotCombined: Set[Scanner] = Set.empty[Scanner]

      remainingScanners.foreach { remScanner =>
        baseScanner.checkOverlap(remScanner) match {
          case Some(OverlapResult(delta, transformation)) =>
            deltas += delta
            val beaconsWithRefToBase: Set[Beacon] = {
              val transformedBeacons: Set[Beacon] =
                remScanner.beacons.map(beacon =>
                  Beacon(transformation(beacon.coord))
                )
              transformedBeacons.map { beacon =>
                val Coord(x, y, z) = beacon.coord
                Beacon(Coord(x - delta.x, y - delta.y, z - delta.z))
              }
            }
            baseScanner =
              Scanner(baseScanner.beacons union beaconsWithRefToBase)

          case None => // could not be combined
            scannersNotCombined += remScanner
        }
      }

      remainingScanners = scannersNotCombined
    }

    baseScanner.beacons.size
  }

  def solutionToSecondHalf: Long = {
    val allDeltas = deltas.toVector

    val allDistances =
      for {
        i <- 0 until allDeltas.size
        j <- (i + 1) until allDeltas.size
      } yield {
        val Delta(x1, y1, z1) = allDeltas(i)
        val Delta(x2, y2, z2) = allDeltas(j)
        math.abs(x1 - x2) + math.abs(y1 - y2) + math.abs(z1 - z2)
      }

    allDistances.max
  }

  val inputScanners: List[Scanner] =
    readAll("day-19-input.txt")
      .split("\n\n")
      .map(s => Scanner.fromString(s).get)
      .toList

  println(solutionToFirstHalf(inputScanners))
  println(solutionToSecondHalf)
}
