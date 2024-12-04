package day4

import scala.io.Source

case class Elem(value: Char)

//case object IrrelevantElem {
//  def apply(): Elem = Elem('.')  // TOFIX
//}

//case class Matrix[T](data: Array[Array[T]])
//object Matrix {
//  def apply(maze: String): Matrix = {
//    val mazeSeq = maze.split("\n").map(row => row.map(char => Elem(char)))
//    new Matrix(mazeSeq)
//  }
//}
//object Reader {
//  def read(filepath: String): String = {
//    val source = Source.fromFile(filepath)
//    val mazeString = source.mkString
//    Matrix(mazeString)
//  }
//}

//object CeresSearch {
//  def searchWord("XMAS", maze: Matrix): Boolean = {
//    val word = "XMAS"
//    val wordLength = word.length
//    val mazeLength = maze.length
//    for (i <- 0 to mazeLength - wordLength) {
//      if (maze.slice(i, i + wordLength) == word) {
//        return true
//      }
//    }
//    false
//  }
//}


object MainDay4 extends App {
  def main(args: Array[String]): Unit = {
    print("Hello, world!")
  }
}
