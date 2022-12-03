case class Rucksack(content: Seq[Int]) {
  private lazy val (leftCompartment, rightCompartment) = content.splitAt(content.length / 2)

  lazy val contentTypes = content.toSet
  lazy val bothCompartmentType = leftCompartment.intersect(rightCompartment).head
}

object Rucksack {
  private def priority(t: Char) = {
    val start = if (t.isLower) 'a'.toInt else 'A'.toInt - 26
    t.toInt - start + 1
  }

  def parse(string: String) = Rucksack(string.toList.map(priority))
}

object RucksackReorganization extends AOCSolution[Int, Int](3) {
  private val rucksacks = inputLines(Rucksack.parse)

  protected val FirstPartAnswer: Int = rucksacks.map(_.bothCompartmentType).sum
  protected val SecondPartAnswer: Int = rucksacks
    .grouped(3)
    .map(group => group.map(_.contentTypes).fold(Set.range(0, 53)) { case (l, r) => l.intersect(r) }.head)
    .sum
}

@main def Day3() = RucksackReorganization.printAnswers()
