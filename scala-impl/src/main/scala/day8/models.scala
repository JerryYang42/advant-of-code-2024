package day8

sealed trait Elem {
  def value: Char
}
object SpaceElem extends Elem {
  override def value: Char = '.'
}
case class AntennaElem(private val char: Char) extends Elem {
  override def value: Char = char
}
object AntennaElem {
  def apply(char: Char): AntennaElem = {
    assert(char.isLetterOrDigit, s"Invalid character: $char")
    new AntennaElem(char)
  }
}
class AntinodeElem extends Elem {
  override def value: Char = '#'
  var generatedBy: Set[AntennaElem] = Set.empty
}

case class Point(x: Int, y: Int) {
  def +(vector: Vector): Point = Point(x + vector.dx, y + vector.dy)
}
case class Vector(dx: Int, dy: Int) {
  def endPoint: Point = Point(dx, dy)
  def *(scalar: Int): Vector = Vector(dx * scalar, dy * scalar)
}
object Vector {
  def apply(startingPoint: Point, endingPoint: Point): Vector = {
    Vector(endingPoint.x - startingPoint.x, endingPoint.y - startingPoint.y)
  }
}

case class Dimension(width: Int, height: Int) {
  def contains(point: Point): Boolean = {
    point.x >= 0 && point.x < width && point.y >= 0 && point.y < height
  }
  val diameter: Int = Math.max(width, height)
}

case class Matrix[Elem](data: Array[Array[Elem]]) {
  def get(x: Int, y: Int): Elem = {
    data(x)(y)
  }
  def set(x: Int, y: Int, elem: Elem): Unit = {
    data(x)(y) = elem
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
  val diameter: Int = dimension.diameter
}

//trait Direction {
//  def dx: Int
//  def dy: Int
//  def opposite: Direction = this match {
//    case Right => Left
//    case Down => Up
//    case DiagonalDownRight => DiagonalUpLeft
//    case DiagonalDownLeft => DiagonalUpRight
//    case DiagonalUpRight => DiagonalDownLeft
//    case DiagonalUpLeft => DiagonalDownRight
//    case Left => Right
//    case Up => Down
//  }
//}
//object Direction {
//  val allDirections: Seq[Direction] = Seq(Right, Down, DiagonalDownRight, DiagonalDownLeft, DiagonalUpRight, DiagonalUpLeft, Left, Up)
//  val diagonalDirections: Seq[Direction] = Seq(DiagonalDownRight, DiagonalDownLeft, DiagonalUpRight, DiagonalUpLeft)
//}
//case object Right extends Direction {
//  override def dx: Int = 1
//  override def dy: Int = 0
//}
//case object Down extends Direction {
//  override def dx: Int = 0
//  override def dy: Int = 1
//}
//case object DiagonalDownRight extends Direction {
//  override def dx: Int = 1
//  override def dy: Int = 1
//}
//case object DiagonalDownLeft extends Direction {
//  override def dx: Int = -1
//  override def dy: Int = 1
//}
//case object DiagonalUpRight extends Direction {
//  override def dx: Int = 1
//  override def dy: Int = -1
//}
//case object DiagonalUpLeft extends Direction {
//  override def dx: Int = -1
//  override def dy: Int = -1
//}
//case object Left extends Direction {
//  override def dx: Int = -1
//  override def dy: Int = 0
//}
//case object Up extends Direction {
//  override def dx: Int = 0
//  override def dy: Int = -1
//}
//
//case class Vector(scalar: Int, direction: Direction) {
//  def dx: Int = scalar * direction.dx
//  def dy: Int = scalar * direction.dy
//}
//
//case class DirectedVector(startingPoint: Point, vector: Vector) {
//  def endPoint: Point = startingPoint + vector
//}