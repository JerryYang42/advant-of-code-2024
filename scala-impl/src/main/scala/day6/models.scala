package day6

import day4.Direction.diagonalDirections

import scala.annotation.targetName


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

case class Matrix[T](data: Array[Array[T]]) {
  def get(x: Int, y: Int): T = {
    data(y)(x)
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

object BoundaryChecker {
  def isOutOfBounds(point: Point, dimension: Dimension): Boolean = {
    point.x < 0 || point.x >= dimension.width || point.y < 0 || point.y >= dimension.height
  }
}
