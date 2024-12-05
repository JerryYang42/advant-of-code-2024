package day4

import day4.BoundaryChecker.{isInboundCentralPoint, isInboundSequence}
import day4.Direction.{allDirections, diagonalDirections}

import scala.io.Source
import scala.language.postfixOps

object Reader {
  def read(filepath: String): Matrix[Elem] = {
    val source = Source.fromFile(filepath)
    val matrixString = source.mkString
    Matrix(matrixString)
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
    val armReach = pattern.length - 1

    val matchedPointDirections = for {
      startingPoint <- matrix.allPoints
      direction <- allDirections
      if isInboundSequence(DirectedVector(startingPoint, Vector(armReach, direction)), matrix.dimension)
      if pattern.`match`(matrix, startingPoint, direction)
    } yield (startingPoint, direction)

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

    val padding: Int = word.length / 2
    val matchedXShapedPoints = for {
      centralPoint <- matrix.allPoints
      if isInboundCentralPoint(centralPoint, padding, matrix.dimension)
      if Pattern(word).matchX(matrix, centralPoint)
    } yield centralPoint
    matchedXShapedPoints.length
  }

  private def validateBoundary(word: String, matrix: Matrix[Elem]): Unit = {
    if (word.isEmpty) {
      throw new IllegalArgumentException("Word must not be empty")
    }

    if (matrix.data.isEmpty) {
      throw new IllegalArgumentException("Matrix must not be empty")
    }

    if (word.length > matrix.diameter) {
      throw new IllegalArgumentException("Word must not be larger than the matrix")
    }
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
