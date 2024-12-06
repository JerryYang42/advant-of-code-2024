package day5

case class Monomer(value: Char) {
  def reactsWith(that: Monomer): Boolean = {
    this.value != that.value && this.value.toLower == that.value.toLower
  }
}

case class Polymer(value: String) {
  def react(): Polymer = {
    if (isReactive) {
      val newValue = value.foldLeft(List.empty[Char]) {
        case (head :: tail, next) if Monomer(head).reactsWith(Monomer(next)) => tail
        case (acc, next) => next :: acc
      }.reverse.mkString
      Polymer(newValue).react()
    } else {
      this
    }
  }

  private def isReactive: Boolean = {
    value.sliding(2).exists { case Seq(a, b) => Monomer(a).reactsWith(Monomer(b)) }
  }
}

object AlchemicalReduction {

}
