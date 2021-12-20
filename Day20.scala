import InputReader._

object Day20 extends App {
  case class Algorithm(value: Vector[Char])
  case class Image(value: Vector[Vector[Char]]) {
    override def toString: String = {
      val intermediate: Vector[String] = value.map(_.mkString)
      intermediate.mkString("\n")
    }
  }

  def pixelsToNum(pixels: Vector[Char]): Int = {
    val binary: Vector[Int] = pixels.map {
      case '#' => 1
      case '.' => 0
    }

    var num: Int = 0
    for {
      idx <- (binary.size - 1) to 0 by -1
    } {
      val position = binary.size - 1 - idx
      val bit: Int = binary(idx)
      num += (bit << position)
    }
    num
  }

  // pad two row to the top and bottom of the image
  def padRow(image: Image, pixel: Char): Image = {
    val imageVal: Vector[Vector[Char]] = image.value

    val newRow: Vector[Char] = (pixel.toString * imageVal(0).size).toVector
    val newImageVal =
      imageVal
        .prepended(newRow)
        .prepended(newRow)
        .appended(newRow)
        .appended(newRow)

    Image(newImageVal)
  }

  // pad two column to both left and right of the image
  def padCol(image: Image, pixel: Char): Image = {
    val imageVal: Vector[Vector[Char]] = image.value

    val newImageVal = imageVal.map(
      _.prepended(pixel).prepended(pixel).appended(pixel).appended(pixel)
    )
    Image(newImageVal)
  }

  def processImage(
      image: Image,
      algorithm: Algorithm,
      defaultPixel: Char
  ): Image = {
    val MAX_ROW_ID = image.value.size - 1
    val MAX_COL_ID = image.value(0).size - 1

    def getNinePixels(rowId: Int, colId: Int): Vector[Char] = {
      val ninePixels: IndexedSeq[Char] =
        for {
          rDelta <- -1 to 1
          cDelta <- -1 to 1
        } yield {
          val currRowId = rowId + rDelta
          val currColId = colId + cDelta

          val rowIdIsValid = (currRowId >= 0 && currRowId <= MAX_ROW_ID)
          val colIdIsValid = (currColId >= 0 && currColId <= MAX_COL_ID)

          if (rowIdIsValid && colIdIsValid) image.value(currRowId)(currColId)
          else defaultPixel
        }

      ninePixels.toVector
    }

    val outputImageSeq: IndexedSeq[Vector[Char]] =
      for {
        rowId <- 0 to MAX_ROW_ID
      } yield {
        val outputRow: IndexedSeq[Char] =
          for {
            colId <- 0 to MAX_COL_ID
          } yield {
            val ninePixels: Vector[Char] = getNinePixels(rowId, colId)
            val algorithmId: Int = pixelsToNum(ninePixels)
            algorithm.value(algorithmId)
          }

        outputRow.toVector
      }

    Image(outputImageSeq.toVector)
  }

  def solutionToFirstHalf(image: Image, algorithm: Algorithm): Int = {
    var processedImage: Image = image
    var defaultPixel: Char = '.'

    (1 to 50).foreach { _ =>
      // The outermost light pixels can cause dark pixels one row above/below OR
      // one col to the left/right of the current image to become light
      // Add one additional row of padding to determine what the default pixel should be for the next round
      processedImage = {
        val withRowPadding: Image = padRow(processedImage, defaultPixel)
        padCol(withRowPadding, defaultPixel)
      }

      processedImage = processImage(processedImage, algorithm, defaultPixel)
      defaultPixel = processedImage.value(0)(0)
    }

    println(processedImage)
    processedImage.value.map(row => row.count(_ == '#')).sum
  }

  val (algorithm: Algorithm, inputImage: Image) = {
    val algoAndImage: List[String] =
      readAll("day-20-input.txt")
        .split("\n\n")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .toList

    algoAndImage match {
      case List(algoStr, imageStr) =>
        val algoLines: List[String] =
          algoStr.split("\n").map(_.trim).filterNot(_.isEmpty).toList
        val algo = Algorithm(algoLines.mkString.toVector)

        val imageLines: List[String] =
          imageStr.split("\n").map(_.trim).filterNot(_.isEmpty).toList
        val image: Image = Image(imageLines.map(_.toVector).toVector)
        (algo, image)
      case _ =>
        throw new IllegalArgumentException(s"input: $algoAndImage is invalid")
    }
  }

  println(solutionToFirstHalf(inputImage, algorithm))
}
