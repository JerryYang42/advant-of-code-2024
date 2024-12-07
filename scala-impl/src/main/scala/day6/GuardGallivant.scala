package day6

trait Tile
case object Obstruction extends Tile
case class Space(private var visited: Boolean = false) extends Tile {
  def markVisited(): Unit = visited = true
}

trait Character extends Tile {
  def position: Point
  def move(map: LabMap): Unit
}
class Guard(private val initialPosition: Point, private val initialDirection: Direction) extends Character {
  private var currentPosition: Point = initialPosition
  private var currentDirection: Direction = initialDirection
  override def position: Point = position
  override def move(map: LabMap): Unit = {
    while (!BoundaryChecker.isOutOfBounds(intendedNextPosition, map.tiles.dimension)) {
      moveOneStep(map)
      val currentTile = map.get(currentPosition)
      currentTile match {
        case Space(_) => currentTile.asInstanceOf[Space].markVisited()
      }
    }
  }
  def moveOneStep(map: LabMap): Unit = {
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

  override def toString: String = {
    tiles.data.map { row =>
      row.map {
        case Obstruction => "#"
        case Space(true) => "X"
        case Space(false) => "."
      }.mkString("")
    }.mkString("\n")
  }

  def numberOfVisitedSpaces: Int = {
    tiles.data.map { row =>
      row.count {
        case Space(true) => true
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
          case 'v' |
               '>' |
               '<' |
               '^' => Space(true)
          case _ => throw new Exception(s"Invalid tile or character $char, expecting '#', '.' or 'v'")
        }
      }.toArray
    }.toArray
    LabMap(Matrix[Tile](tiles))
  }
}

case class LabMapRepresentation(lines: List[String]) {
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
}

object Reader {
  def read(filepath: String): LabMapRepresentation = {
    val bufferedSource = io.Source.fromResource(filepath)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
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
  def patrolRouteCoverage(): Unit = {
    val labMapRepr = Reader.read("day6/puzzle-input.txt")
    val labMap = LabMap(labMapRepr)
    val guard = labMapRepr.initGuard()
    guard.move(labMap)
    println(s"Number of distinct positions the guard visited on the map: ${labMap.numberOfVisitedSpaces}")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    GuardGallivant.patrolRouteCoverage()
  }
}
