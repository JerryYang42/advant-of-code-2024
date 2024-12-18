package day8

case class AntennaBoard(matrix: Matrix[Elem]) {
  def newBlankAntinodeBoard: AntinodeBoard = {
    AntinodeBoard(Matrix(Array.fill(matrix.height, matrix.width)(SpaceElem)))
  }

  def antennas: Set[AntennaElem] = {
    matrix.data.flatten.collect {
      case elem: AntennaElem => elem
    }.toSet
  }
}
case class AntinodeBoard(matrix: Matrix[Elem]) {
  def antinodesCount: Int = {
    matrix.data.flatten.collect {
      case elem: AntinodeElem => elem
    }.length
  }
}

object Reader {
  def readAntennaBoard(filepath: String): AntennaBoard = {
    val source = scala.io.Source.fromResource(filepath)
    val lines = source.getLines().toList
    val matrix = Matrix(lines.map { line =>
      line.map {
        case '.' => SpaceElem
        case char => AntennaElem(char)
      }.toArray
    }.toArray)
    AntennaBoard(matrix)
  }
}

object ResonantCollinearity {

  def revealAntinodes(board: AntennaBoard): AntinodeBoard = {
    val antennas = board.antennas
    val antennaPositionsMap: Map[AntennaElem, Seq[Point]] = antennas.map { antenna =>
      antenna -> board.matrix.allPoints.filter { point =>
        board.matrix.get(point.x, point.y) == antenna
      }
    }.toMap
    val antennaPositionPairsMap: Map[AntennaElem, Seq[(Point, Point)]] = antennaPositionsMap
      .filter { case (_, points) => points.length >= 2 }
      .map { case (antenna, points) =>
        antenna -> points.combinations(2).toSeq
      }.map {case (antenna, pair) =>
        antenna -> pair.map { case Seq(point1, point2) => (point1, point2) }
      }

    val antinodeBoard = board.newBlankAntinodeBoard
    for {
      (antenna, pairs) <- antennaPositionPairsMap
      (point1, point2) <- pairs
    } {
      val potentialAntinodes = Seq(
        point2 + Vector(point1, point2),
        point1 + Vector(point2, point1))
      potentialAntinodes.foreach { potentialAntinode =>
        try {
          if (antinodeBoard.matrix.get(potentialAntinode.x, potentialAntinode.y) == SpaceElem) {
            val newAntinode = new AntinodeElem
            newAntinode.generatedBy += antenna
            antinodeBoard.matrix.set(potentialAntinode.x, potentialAntinode.y, newAntinode)
          } else if (antinodeBoard.matrix.get(potentialAntinode.x, potentialAntinode.y) != SpaceElem) {
            val existingAntinode = antinodeBoard.matrix.get(potentialAntinode.x, potentialAntinode.y).asInstanceOf[AntinodeElem]
            existingAntinode.generatedBy += antenna
          }
        } catch {
          case _: ArrayIndexOutOfBoundsException => ()
        }
      }
    }
    antinodeBoard
  }

  def main(args: Array[String]): Unit = {
    val board = Reader.readAntennaBoard("day8/quiz-board.txt")
    val antinodeBoard = revealAntinodes(board)
    val result = antinodeBoard.antinodesCount
    print(s"There are $result total unique locations that contain an antinode within the bounds of the map")
  }
}
