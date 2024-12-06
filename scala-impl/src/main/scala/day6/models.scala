package day6

import day4.Direction.diagonalDirections

import scala.annotation.targetName
//
//trait Elem {
//  def value: Char
//}
//object X extends Elem {
//  override def value: Char = 'X'
//}
//object M extends Elem {
//  override def value: Char = 'M'
//}
//object A extends Elem {
//  override def value: Char = 'A'
//}
//object S extends Elem {
//  override def value: Char = 'S'
//}
//object IrrelevantElem extends Elem {
//  override def value: Char = '.'
//}
//object Elem {
//  def apply(char: Char): Elem = {
//    char match {
//      case 'X' => X
//      case 'M' => M
//      case 'A' => A
//      case 'S' => S
//      case '.' => IrrelevantElem
//      case _ => throw new IllegalArgumentException(s"Invalid character: $char")
//    }
//  }
//}
//
//case class Pattern(pattern: Array[Elem]) {
//  def length: Int = pattern.length
//
//  def `match`(matrix: Matrix[Elem], startingPoint: Point, direction: Direction): Boolean = {
//    val (x, y) = (startingPoint.x, startingPoint.y)
//    val (dx, dy) = (direction.dx, direction.dy)
//    pattern.zipWithIndex.forall { case (elem, i) =>
//      val (newX, newY) = (x + i * dx, y + i * dy)
//      matrix.get(newX, newY) == elem
//    }
//  }
//
//  def matchX(matrix: Matrix[Elem], point: Point): Boolean = {
//    val center = CentralPoint(point.x, point.y)
//    val radius = pattern.length / 2
//    center.diagonalEndPoints(radius).count { (point, direction) =>
//      `match`(matrix, point, direction.opposite)
//    } == 2
//  }
//}
//
//object Pattern {
//  def apply(patternString: String): Pattern = {
//    new Pattern(patternString.map(char => Elem(char)).toArray)
//  }
//}

case class Point(x: Int, y: Int) {
  def +(vector: Vector): Point = Point(x + vector.dx, y + vector.dy)
}
class CentralPoint(x: Int, y: Int) extends Point(x, y) {
  def diagonalEndPoints(length: Int): Seq[(Point, Direction)] = {
    for {
      direction <- Direction.diagonalDirections
    } yield {
      val vector = Vector(length, direction)
      (this + vector, direction)
    }
  }
}

case class Dimension(width: Int, height: Int)

case class Matrix[Elem](data: Array[Array[Elem]]) {
  def get(x: Int, y: Int): Elem = {
    data(x)(y)
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
  val diameter: Int = Math.max(width, height)
}

object Matrix {
  /**
   * Create a matrix from a string
   * The string should be in the format of
   * M.S
   * .A.
   * M.S
   * @param matrix the string representation of the matrix
   * @tparam T the type of the element in the matrix
   * @return
   */
  def apply[T](matrix: String): Matrix[Elem] = {
    val data = matrix.split("\n").map(row => row.map(char => Elem(char)).toArray)
    data.ensuring(mat => mat.forall(row => row.length == data.head.length), "All rows must have the same length")
    new Matrix(data)
  }
}

trait Direction {
  def dx: Int
  def dy: Int
  def opposite: Direction = this match {
    case Right => Left
    case Down => Up
    case DiagonalDownRight => DiagonalUpLeft
    case DiagonalDownLeft => DiagonalUpRight
    case DiagonalUpRight => DiagonalDownLeft
    case DiagonalUpLeft => DiagonalDownRight
    case Left => Right
    case Up => Down
  }
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

case class Vector(scalar: Int, direction: Direction) {
  def dx: Int = scalar * direction.dx
  def dy: Int = scalar * direction.dy
}

case class DirectedVector(startingPoint: Point, vector: Vector) {
  def endPoint: Point = startingPoint + vector
}