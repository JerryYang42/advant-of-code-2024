package day4

import day4.Direction.diagonalDirections

object BoundaryChecker {
  def isInboundCentralPoint(centralPoint: Point, padding: Int, dimension: Dimension): Boolean = {
    diagonalDirections.map(direction => {
      val directedVector = DirectedVector(centralPoint, Vector(padding, direction))
      isInboundSequence(directedVector, dimension)
    }).forall(identity)
  }

  def isInboundSequence(directedVector: DirectedVector, dimension: Dimension): Boolean = {
    isInboundPoint(directedVector.endPoint, dimension)
  }

  private def isInboundPoint(point: Point, dimension: Dimension): Boolean = {
    point.x >= 0 && point.x < dimension.width && point.y >= 0 && point.y < dimension.height
  }
}
