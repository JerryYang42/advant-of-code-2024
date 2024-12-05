import day4.Direction.diagonalDirections
import day4.{Matrix, Point}

package object day4 {
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

  case class Pattern(pattern: Array[Elem]) {
    def length: Int = pattern.length

    def `match`(matrix: Matrix[Elem], startingPoint: Point, direction: Direction): Boolean = {
      val height = matrix.data.length
      val width = matrix.data.head.length
      val (x, y) = (startingPoint.x, startingPoint.y)
      val (dx, dy) = (direction.dx, direction.dy)
      pattern.zipWithIndex.forall { case (elem, i) =>
        val (newX, newY) = (x + i * dx, y + i * dy)
        matrix.get(Point(newX, newY)) == elem
      }
    }

    def matchX(matrix: Matrix[Elem], centerPoint: Point): Boolean = {
      val height = matrix.data.length
      val width = matrix.data.head.length
      val (x, y) = (centerPoint.x, centerPoint.y)
      diagonalDirections.map { direction =>
        val (dx, dy) = (direction.dx, direction.dy)
        pattern.zip(Seq(1, 0, -1)).forall { case (elem, i) =>
          val (newX, newY) = (x + i * dx, y + i * dy)
          matrix.get(Point(newX, newY)) == elem
        }
      }.count(identity) == 2
    }
  }

  object Pattern {
    def apply(patternString: String): Pattern = {
      new Pattern(patternString.map(char => Elem(char)).toArray)
    }
  }

  case class Point(x: Int, y: Int)

  case class Dimension(width: Int, height: Int)

  case class Matrix[Elem](data: Array[Array[Elem]]) {
    def get(point: Point): Elem = {
      data(point.x)(point.y)
    }

    def allPoints: Seq[Point] = {
      for {
        y <- data.indices
        x <- data(y).indices
      } yield Point(x, y)
    }

    val height: Int = data.length
    val width: Int = data.head.length
    val dimension: Dimension = Dimension(width, height)
  }

  object Matrix {
    def apply[T](maze: String): Matrix[Elem] = {
      val data = maze.split("\n").map(row => row.map(char => Elem(char)).toArray)
      data.ensuring(mat => mat.forall(row => row.length == data.head.length), "All rows must have the same length")
      new Matrix(data)
    }
  }
  trait Direction {
    def dx: Int
    def dy: Int
  }
  object Direction {
    val allDirections: Seq[Direction] = Seq(Right, Down, DiagonalDownRight, DiagonalDownLeft, DiagonalUpRight, DiagonalUpLeft, Left, Up)
    val diagonalDirections: Seq[Direction] = Seq(DiagonalDownRight, DiagonalDownLeft, DiagonalUpRight, DiagonalUpLeft)
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
}
