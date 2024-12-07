package day6

import scala.collection.mutable.ArrayBuffer

case class VisitedSnapshot(var direction: Option[Direction]) {
  def isVisited: Boolean = direction.isDefined
  def markVisited(direction: Direction): Unit = this.direction = Some(direction)
  def isLoopingRoute(direction: Direction): Boolean = isVisited && direction == this.direction.get
}
trait Tile
case object Obstruction extends Tile
case class Space(private var visitedSnapshot: VisitedSnapshot = VisitedSnapshot(None)) extends Tile {
  def isVisited: Boolean = visitedSnapshot.isVisited
  def markVisited(direction: Direction): Unit = visitedSnapshot.markVisited(direction)
  def isLoopingRoute(direction: Direction): Boolean = visitedSnapshot.isLoopingRoute(direction)
}

trait MovementResult
case class OutOfBounds(position: Point, direction: Direction) extends MovementResult
case class LoopingRoute(position: Point, direction: Direction) extends MovementResult

trait Character extends Tile {
  def position: Point
  def move(map: LabMap): MovementResult
}
class Guard(private val initialPosition: Point, private val initialDirection: Direction) extends Character {
  private var currentPosition: Point = initialPosition
  private var currentDirection: Direction = initialDirection
  override def position: Point = position
  override def move(map: LabMap): MovementResult = {
    while (!BoundaryChecker.isOutOfBounds(intendedNextPosition, map.tiles.dimension)) {
      moveOneStep(map)
      val currentTile = map.get(currentPosition)
      currentTile match {
        case Space(_) =>
          val currentSpace = currentTile.asInstanceOf[Space]
          if (currentSpace.isLoopingRoute(currentDirection)) {
            return LoopingRoute(currentPosition, currentDirection)
          }
          currentSpace.markVisited(currentDirection)
      }
    }
    OutOfBounds(currentPosition, currentDirection)
  }
  private def moveOneStep(map: LabMap): Unit = {
    val nextTile = map.get(intendedNextPosition)
    nextTile match {
      case Obstruction =>
        turnRight()
      case Space(_) =>
        stepForward()
      case _ => throw new Exception(s"Invalid tile ${nextTile.getClass}")
    }
  }
  private def intendedNextPosition: Point = currentPosition + Vector(1, currentDirection)
  private def stepForward(): Unit = currentPosition = currentPosition + Vector(1, currentDirection)
  private def turnRight(): Unit = currentDirection = currentDirection match {
    case Right => Down
    case Down => Left
    case Left => Up
    case Up => Right
    case _ => throw new Exception(s"Invalid direction $currentDirection")
  }
}

case class LabMap(tiles: Matrix[Tile]) {
  def get(position: Point): Tile = tiles.get(position.x, position.y)

  def toReprV1: LabMapRepresentation = {
    val lines = tiles.data.map { row =>
      row.map {
        case Obstruction => "#"
        case Space(visitedSnapshot) => visitedSnapshot.direction match {
          case Some(Down) => "v"
          case Some(Right) => ">"
          case Some(Left) => "<"
          case Some(Up) => "^"
          case None => "."
        }
      }.mkString("")
    }
    LabMapRepresentation(lines)
  }

  def numberOfVisitedSpaces: Int = {
    tiles.data.map { row =>
      row.count {
        case Space(visitedSnapshot) if visitedSnapshot.isVisited => true
        case _ => false
      }
    }.sum
  }
}
object LabMap {
  def apply(labMapRepr: LabMapRepresentation): LabMap = {
    val tiles = labMapRepr.lines.map { case line =>
      line.map { case char =>
        char match {
          case '#' => Obstruction
          case '.' => Space()
          case 'v' => Space(VisitedSnapshot(Some(Down)))
          case '>' => Space(VisitedSnapshot(Some(Right)))
          case '<' => Space(VisitedSnapshot(Some(Left)))
          case '^' => Space(VisitedSnapshot(Some(Up)))
          case _ => throw new Exception(s"Invalid tile or character $char, expecting '#', '.' or 'v'")
        }
      }.toArray
    }
    LabMap(Matrix[Tile](tiles))
  }
}

case class LabMapRepresentation(lines: Array[String]) {
  def initGuard(): Guard = {
    val guards = lines.zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.map { case (char, x) =>
        char match {
          case 'v' => Guard(Point(x, y), Down)
          case '<' => Guard(Point(x, y), Left)
          case '>' => Guard(Point(x, y), Right)
          case '^' => Guard(Point(x, y), Up)
          case _ => null
        }
      }
    }
    guards.flatten.filter(_ != null).head
  }

  override def toString: String = lines.mkString("\n")
}

object Reader {
  def read(filepath: String): LabMapRepresentation = {
    val bufferedSource = io.Source.fromResource(filepath)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toArray
    bufferedSource.close
    LabMapRepresentation(lines)
  }
}

object BoundaryChecker {
  def isOutOfBounds(point: Point, dimension: Dimension): Boolean = {
    point.x < 0 || point.x >= dimension.width || point.y < 0 || point.y >= dimension.height
  }
}

object GuardGallivant {
  def patrolRouteCoverage(filepath: String): Unit = {
    val labMapRepr = Reader.read(filepath)
    val labMap = LabMap(labMapRepr)
    val guard = labMapRepr.initGuard()
    guard.move(labMap)
    println(s"Number of distinct positions the guard visited on the map: ${labMap.toReprV1}")
  }

//  def findAllLoopingRouteAfterAddingOneObstacle(filepath: String): Unit = {
//    val labMapRepr = Reader.read(filepath)
//    val labMap = LabMap(labMapRepr)
//    val guard = labMapRepr.initGuard()
//    val loopingRoutes = ArrayBuffer[LoopingRoute]()
//    while (loopingRoutes.isEmpty) {
//      val result = guard.move(labMap)
//      result match {
//        case LoopingRoute(position, direction) =>
//          loopingRoutes += LoopingRoute(position, direction)
//        case OutOfBounds(_, _) =>
//          throw new Exception("Guard went out of bounds")
//      }
//    }
//    println(s"First looping route found: ${loopingRoutes.head}")
//  }
}

object Main {
  def main(args: Array[String]): Unit = {
    GuardGallivant.patrolRouteCoverage("day6/puzzle-input.txt")
//    GuardGallivant.findAllLoopingRouteAfterAddingOneObstacle("day6/map-1.txt")
  }
}
