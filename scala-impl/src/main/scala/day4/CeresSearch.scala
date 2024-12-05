package day4

import day4.Direction.{allDirections, diagonalDirections}

import scala.io.Source
import scala.language.postfixOps

object Reader {
  def read(filepath: String): Matrix[Elem] = {
    val source = Source.fromFile(filepath)
    val mazeString = source.mkString
    Matrix(mazeString)
  }
}

object CeresSearch {
  /**
   * search for a word in a matrix, This word search allows words to be
   * - horizontal,
   * - vertical,
   * - diagonal,
   * - written backwards,
   * - or even overlapping other words.
   * @param word the word to search
   * @param matrix the matrix to search in
   * @return the occurrence of the word in the matrix
   */
  def searchWord(word: String = "XMAS", matrix: Matrix[Elem]): Int = {
    validateBoundary(word, matrix)

    val pattern = Pattern(word)
    val pointDirections = for {
      point <- matrix.allPoints
      direction <- allDirections
    } yield (point, direction)

    val height = matrix.data.length
    val width = matrix.data.head.length
    val inboundPointDirections = pointDirections.filter { case (point, direction) =>
      isInboundSequence(point, pattern.length, direction, height, width)
    }

    val matchedPointDirections = inboundPointDirections.filter { case (point, direction) =>
      pattern.`match`(matrix, point, direction)
    }

    matchedPointDirections.length
  }

  /**
   * search for a word in a matrix, This word currently only supports "MAS"
   *
   * @param word the word to search
   * @param matrix the matrix to search in
   * @return
   */
  def searchXShapedWord(word: String = "MAS", matrix: Matrix[Elem]): Int = {
    validateBoundary(word, matrix)
    if (word != "MAS") {
      throw new IllegalArgumentException("Word must be MAS")
    }

    val height = matrix.data.length
    val width = matrix.data.head.length
    val padding: Int = word.length / 2
    val pointsInBound = matrix.allPoints.filter { startingPoint =>
      isInboundCentralPoint(startingPoint, padding, height, width)
    }
    val pattern = Pattern(word)
    val matchedPoints = pointsInBound.filter { point =>
      pattern.matchX(matrix, point)
    }
    matchedPoints.length
  }

  private def validateBoundary(word: String, matrix: Matrix[Elem]): Unit = {
    if (word.isEmpty) {
      throw new IllegalArgumentException("Word must not be empty")
    }

    if (matrix.data.isEmpty) {
      throw new IllegalArgumentException("Matrix must not be empty")
    }

    val maxSizeOfMatrix = {
      val height = matrix.data.length
      val width = matrix.data.head.length
      Math.max(height, width)
    }
    if (word.length > maxSizeOfMatrix) {
      throw new IllegalArgumentException("Word must not be larger than the matrix")
    }
  }

  private def isInboundSequence(startingPoint: Point, strLength: Int, direction: Direction, height: Int, width: Int): Boolean = {
    val endX = startingPoint.x + (strLength - 1) * direction.dx
    val endY = startingPoint.y + (strLength - 1) * direction.dy
    isInboundPoint(Point(endX, endY), height, width)
  }

  private def isInboundCentralPoint(startingPoint: Point, padding: Int, height: Int, width: Int): Boolean = {
    val cornerPoints = diagonalDirections.map(direction => {
      Point(startingPoint.x + padding * direction.dx, startingPoint.y + padding * direction.dy)
    })
    cornerPoints.map { endPoint =>
      isInboundPoint(endPoint, height, width)
    }.forall(identity)
  }

  private def isInboundPoint(point: Point, height: Int, width: Int): Boolean = {
    point.x >= 0 && point.x < width && point.y >= 0 && point.y < height
  }
}


object MainDay4 {
  def main(args: Array[String]): Unit = {
    val filepath = "src/main/resources/day4/large-matrix.txt"
    val mat = Reader.read(filepath)
    val matches = CeresSearch.searchWord("XMAS", mat)
    println(s"XMAS matches: $matches")
    val matchesX = CeresSearch.searchXShapedWord("MAS", mat)
    println(s"X-MAS matches: $matchesX")
  }
}
