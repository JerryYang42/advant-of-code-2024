package day6

trait Tile {
  def position: Point
}
case class Obstruction(override val position: Point) extends Tile
class Guard(private val initialPosition: Point, private val initialDirection: Direction) extends Tile {
  private var currentPosition: Point = initialPosition
  private var currentDirection: Direction = initialDirection
  override def position: Point = position
  def intendedNextPosition: Point = currentPosition + currentDirection
  def stepForward(): Unit = currentPosition = currentPosition + currentDirection
  def turnRight(): Unit = currentDirection = currentDirection match {
    case Right => Down
    case Down => Left
    case Left => Up
    case Up => Right
    case _ => throw new Exception(s"Invalid direction $currentDirection")
  }
}
case class Map(private val tiles: Matrix[Tile]) {
  def get(position: Point): Tile = tiles(position.y)(position.x)
  def update(position: Point, tile: Tile): Unit = tiles(position.y)(position.x) = tile
}

class GameLoop(private val map: Map, private val guard: Guard) {
  def run(): Unit = {
    while (true) {
      val nextTile = map.get(guard.intendedNextPosition)
      currentTile match {
        case Obstruction(_) => guard.turnRight()
        case _ => guard.stepForward()
      }
    }
  }
}


object Reader {
  def readInput(): List[String] = {
    val bufferedSource = io.Source.fromFile("src/resources/input_day6.txt")
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }
}
object GuardGallivant {

}

