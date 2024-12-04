package day2

import scala.io.Source

class Report(levels: Seq[Int]) {
  def isSafe: Boolean = {
    (isInAscendingOrder || isInDescendingOrder) && allGapsWithinRange(1, 3)
  }

  def isNearlySafe: Boolean = {
    findFirstRemovableLevel match {
      case Some(removableLevel) =>
        val levelsWithoutRemovableLevel = levels.patch(removableLevel, Nil, 1)
      case None =>
        isSafe
    }

  }
  private def findFirstRemovableLevel: Option[Int] = {
    val offset = 1
    val maybeRemovableLevel: Int = levels
      .sliding(3)
      .map { case Seq(a, b, c) => (a - b) * (b - c) < 0 }
      .indexOf(true)

    maybeRemovableLevel match {
      case -1 => // Not found
        None
      case removableLevel =>
        Some(removableLevel + offset)
    }
  }

  // remove the index-th level from the seq and return the new seq
  private[day2] def remove(index: Int): Seq[Int] = {
    levels.patch(index, Nil, 1)
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

object MainDay2 {
  def main(args: Array[String]): Unit = {
    val filepath = "scala-impl/resource/day2/reports_of_levels.txt"
    val reports = Reader.read(filepath)
    val safeReports = RedNosedReports.analyse(reports)
    println(safeReports)
  }
}