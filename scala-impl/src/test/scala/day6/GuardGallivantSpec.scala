package day6

import org.scalatest.flatspec.AnyFlatSpec

import java.util.Arrays
import scala.reflect.{ClassManifest, ClassTag}

class GuardGallivantSpec extends AnyFlatSpec {

  def deepCopyArray[T: ClassTag](array: Array[Array[T]]): Array[Array[T]] = {
    array.map(_.clone())
  }

  behavior of "deep copy"

  it should "copy Guard" in {
    // Example usage
    val originalArray: Array[Array[Int]] = Array(Array(1, 2, 3), Array(4, 5, 6))
    val copiedArray: Array[Array[Int]] = deepCopyArray(originalArray)

    // Verify the deep copy
    println(Arrays.deepEquals(originalArray.asInstanceOf[Array[AnyRef]], copiedArray.asInstanceOf[Array[AnyRef]])) // Output: true
    println(originalArray(0) eq copiedArray(0)) // Output: false
  }

  behavior of "LabMap deepcopy"

  it should "copy LabMap" in {
    val labMapRepr = Reader.read("day6/map-1.txt")
    val labMap = LabMap(labMapRepr)

    val positions = Seq(Point(0, 0), Point(0, 1))
    val mutantMaps = positions.map { p => labMap.newMutantByAddingOneObstacle(p) }
    mutantMaps
//    Array.deepEquals()
  }
}
