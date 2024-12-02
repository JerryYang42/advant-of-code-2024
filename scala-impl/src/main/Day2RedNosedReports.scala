package main

import java.io.File
import scala.io.Source

class Report(levels: Seq[Int]) {
  def isSafe: Boolean = {
    (isInAscendingOrder || isInDescendingOrder) && allGapsWithinRange(1, 3)
  }

  private def isInAscendingOrder: Boolean = levels == levels.sorted

  private def isInDescendingOrder: Boolean = levels == levels.sorted.reverse

  private def allGapsWithinRange(lower: Int, higher: Int): Boolean = {
    if (lower <= 0 || higher <= 0) {
      throw new IllegalArgumentException("Lower and higher must be positive")
    }
    if (lower >= higher) {
      throw new IllegalArgumentException("Lower must be less than higher")
    }
    val gaps = levels
      .sliding(2)
      .map { case Seq(a, b) => b - a }
      .map { Math.abs }
    gaps.forall( gap => { gap >= lower && gap <= higher })
  }
}

object Report {
  def apply(input: String): Report = {
    val levels: Seq[Int] = input.split(" ").map(_.toInt).toSeq
    apply(levels)
  }

  def apply(levels: Seq[Int]): Report = {
    validate(levels)
    new Report(levels)
  }

  private def validate(levels: Seq[Int]): Unit = {
    if (levels.isEmpty) {
      throw new IllegalArgumentException("Report must have at least one level")
    }
  }
}

object Reader {
  def read(filepath: String): Seq[Report] = {
    val source = Source.fromFile(filepath)
    val reports = source.getLines().map(Report.apply).toSeq
//    source.close()
    reports
  }
}

object RedNosedReports {
  /**
   * Analyse the reports and return the number of safe reports
   * @param input of all reports
   * @return the number of safe reports
   */
  def analyse(input: Seq[Report]): Int = {
    input.count(_.isSafe)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val filepath = "scala-impl/resource/reports_of_levels.txt"
    val reports = Reader.read(filepath)
    val safeReports = RedNosedReports.analyse(reports)
    println(safeReports)
  }
}