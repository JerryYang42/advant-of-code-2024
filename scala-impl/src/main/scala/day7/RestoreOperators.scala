package day7

import scala.annotation.tailrec

trait Operator {
  def apply(a: Long, b: Long): Long
}
object AddOperator extends Operator {
  override def apply(a: Long, b: Long): Long = a + b
}
object MultiplyOperator extends Operator {
  override def apply(a: Long, b: Long): Long = a * b
}
object ConcatenateOperator extends Operator {
  override def apply(a: Long, b: Long): Long = (a.toString + b.toString).toLong
}
object Operators {
  val operators: List[Operator] = List(AddOperator, MultiplyOperator, ConcatenateOperator)
}

object BinarySet {
  def generateLists[A](values: List[A], length: Long): List[List[A]] = {
    if (length == 0) List(Nil)
    else values.flatMap { value =>
      generateLists(values, length - 1).map(value :: _)
    }
  }
}

case class Operands(operands: List[Long]) {
  def possibleStatements: List[Statement] = {
    assert(operands.nonEmpty, "numbers should not be empty")
    val operatorsSets = BinarySet.generateLists[Operator](Operators.operators, operands.length - 1)
    val statements = operatorsSets.map { operators => Statement(this, operators) }
    statements
  }

  def length: Long = operands.length
}
/**
 * A statemenet is an algorithmic computable numbers and operators that outputs a Longeger result
 */
case class Statement(operands: Operands, operators: List[Operator]) {
  assert(operands.length == operators.length + 1, s"length numbers should outnumber length of operators by exact 1, got ${operands.length} and ${operators.length}")
  assert(operands.length >= 2, "requires 2 or more operands to form a statement")

  def equalTo(target: Long): Boolean = result == target

  private def result: Long = {
    val operandsHead = operands.operands.head
    val operatorsHead = operators.head
    operands.operands.tail.zipAll(operators.tail, operandsHead, operatorsHead).foldLeft(operandsHead) {
      case (acc, (number, operator)) => operator(acc, number)
    }
  }
}

object Reader {
  def read(filepath: String): List[(List[Long], Long)] = {
    val source = scala.io.Source.fromResource(filepath)
    val lines = source.getLines().toList
    source.close()
    lines.map { line =>
      val Array(targetStr, operandsStr) = line.split(":")
      val target: Long = targetStr.toLong
      val operands: List[Long] = operandsStr.trim.split(" ").map(_.toLong).toList
      (operands, target)
    }
  }
}

object RestoreOperators {

  def canRestoreOperators(input: List[Long], target: Long): Boolean = {
    val operands = Operands(input)
    val statements = operands.possibleStatements
    statements.exists(_.equalTo(target))
  }

  def main(args: Array[String]): Unit = {
    // "day7/mini-input.txt" -> part 1: 3749
    // "day7/mini-input.txt" -> part 2: 11387
    val tuples = Reader.read("day7/puzzle-input.txt")
    val result = tuples
      .filter {
        case (input, target) => canRestoreOperators(input, target)
      }.map {
        case (input, target) => target
      }.sum
    println(s"The total calibration result is $result")
  }
}
