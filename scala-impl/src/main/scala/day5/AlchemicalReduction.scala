package day5

import scala.annotation.tailrec

case class Monomer(value: Char) {
  def reactsWith(that: Monomer): Boolean = {
    this.value != that.value && this.value.toLower == that.value.toLower
  }
}

case class Polymer(value: Array[Monomer]) {
  def react(): Polymer = {
    if (isReactive) {
      val newValue = value.foldLeft(List.empty[Monomer]) {
        case (head :: tail, next) if head.reactsWith(next) => tail
        case (acc, next) => next :: acc
      }.reverse
      Polymer(newValue)
    } else {
      this
    }
  }

  private def isReactive: Boolean = {
    value.sliding(2).exists { case Array(a: Monomer, b: Monomer) => a.reactsWith(b) }
  }

  override def toString: String = value.map(_.value).mkString
}
object Polymer {
  def apply(value: String): Polymer = Polymer(value.toCharArray.map(Monomer.apply))
  def apply(value: List[Monomer]): Polymer = {
    val newValue: Array[Monomer] = value.toArray
    new Polymer(newValue)
  }
}

object Reader {
  def readPolymer(): Polymer = {
    val source = io.Source.fromResource("day5/puzzle-input.txt")
    try {
      Polymer(source.mkString)
    } finally {
      source.close()
    }
  }
}

object AlchemicalReduction {
  def main(args: Array[String]): Unit = {
    val polymer = Reader.readPolymer()
//    println(s"The original polymer: $polymer")
    println(s"The reacted polymer have length:  ${polymer.react().value.length}")
  }
}
