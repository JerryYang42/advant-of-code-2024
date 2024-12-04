package day4

import scala.io.Source

trait Elem {
  def value: Char
}
object X extends Elem {
  override def value: Char = 'X'
}
object M extends Elem {
  override def value: Char = 'M'
}
object A extends Elem {
  override def value: Char = 'A'
}
object S extends Elem {
  override def value: Char = 'S'
}
object IrrelevantElem extends Elem {
  override def value: Char = '.'
}
object Elem {
  def apply(char: Char): Elem = {
    char match {
      case 'X' => X
      case 'M' => M
      case 'A' => A
      case 'S' => S
      case '.' => IrrelevantElem
      case _ => throw new IllegalArgumentException(s"Invalid character: $char")
    }
  }
}

case class Point(x: Int, y: Int)

case class Matrix[Elem](data: Array[Array[Elem]]) {
//  override def toString: String = {
//    data.map(row => row.map(_.value).mkString).mkString("\n")
//  }

  def get(point: Point): Elem = {
    data(point.x)(point.y)
  }

  def allPoints: Seq[Point] = {
    for {
      y <- data.indices
      x <- data(y).indices
    } yield Point(x, y)
  }
}
object Matrix {
  def apply[T](maze: String): Matrix[Elem] = {
    val data = maze.split("\n").map(row => row.map(char => Elem(char)).toArray)
    data.ensuring(mat => mat.forall(row => row.length == data.head.length), "All rows must have the same length")
    new Matrix(data)
  }
}

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
    // if the word is empty, return 0
    if (word.isEmpty) {
      return 0
    }
    // if the matrix is empty, return 0
    if (matrix.data.isEmpty) {
      return 0
    }
    // if the word outsized the matrix, return 0
    val maxSizeOfMatrix = {
      val height = matrix.data.length
      val weight = matrix.data.head.length
      Math.max(height, weight)
    }
    if (word.length > maxSizeOfMatrix) {
      return 0
    }

    val pattern: Array[Elem] = word.map(char => Elem(char)).toArray
    val pointDirections = for {
      point <- matrix.allPoints
      direction <- directions
    } yield (point, direction)

    val height = matrix.data.length
    val weight = matrix.data.head.length
    val inboundPointDirections = pointDirections.filter { case (point, direction) =>
        val endX = point.x + (pattern.length - 1) * direction.dx
        val endY = point.y + (pattern.length - 1) * direction.dy
        pointWithinBounds(Point(endX, endY), height, weight)
    }

    val matchedPointDirections = inboundPointDirections.filter { case (point, direction) =>
      directionsWithExactMatch(pattern, matrix, point, direction)
    }

    matchedPointDirections.length
  }

  trait Direction {
    def dx: Int
    def dy: Int
  }
  case object Right extends Direction {
    override def dx: Int = 1
    override def dy: Int = 0
  }
  case object Down extends Direction {
    override def dx: Int = 0
    override def dy: Int = 1
  }
  case object DiagonalDownRight extends Direction {
    override def dx: Int = 1
    override def dy: Int = 1
  }
  case object DiagonalDownLeft extends Direction {
    override def dx: Int = -1
    override def dy: Int = 1
  }
  case object DiagonalUpRight extends Direction {
    override def dx: Int = 1
    override def dy: Int = -1
  }
  case object DiagonalUpLeft extends Direction {
    override def dx: Int = -1
    override def dy: Int = -1
  }
  case object Left extends Direction {
    override def dx: Int = -1
    override def dy: Int = 0
  }
  case object Up extends Direction {
    override def dx: Int = 0
    override def dy: Int = -1
  }
  private val directions = Seq(Right, Down, DiagonalDownRight, DiagonalDownLeft, DiagonalUpRight, DiagonalUpLeft, Left, Up)

  private val pointWithinBounds: (Point, Int, Int) => Boolean = (point, height, width) => {
    point.x >= 0 && point.x < width && point.y >= 0 && point.y < height
  }
  private val directionsWithExactMatch: (Array[Elem], Matrix[Elem], Point, Direction) => Boolean = (pattern, matrix, startingPoint, direction) => {
    val height = matrix.data.length
    val weight = matrix.data.head.length
    val (x, y) = (startingPoint.x, startingPoint.y)
    val (dx, dy) = (direction.dx, direction.dy)
    pattern.zipWithIndex.forall { case (elem, i) =>
      val (newX, newY) = (x + i * dx, y + i * dy)
      matrix.get(Point(newX, newY)) == elem
    }
  }
}


object MainDay4 {
  def main(args: Array[String]): Unit = {
    //    val mat = Matrix[Elem]("MA\nCA")
    val filepath = "src/main/resources/day4/large-matrix.txt"
    val mat = Reader.read(filepath)
    val matches = CeresSearch.searchWord("XMAS", mat)
    print("matches: ", matches)
  }
}
