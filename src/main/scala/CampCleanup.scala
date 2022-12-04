case class Range(start: Int, end: Int) {
  def fullyContains(other: Range) = start <= other.start && end >= other.end
  def overlaps(other: Range) = other.start <= start && start <= other.end || start <= other.start && other.start <= end
}

object CampCleanup extends AOCSolution[Int, Int](4) {
  val rangePairs = inputLines(_.split(',').toSeq.map(_.split('-').map(_.toInt)).map(range => Range(range.head, range.last)))

  protected val FirstPartAnswer: Int = rangePairs.count { case Seq(l, r) => l.fullyContains(r) || r.fullyContains(l) }
  protected val SecondPartAnswer: Int = rangePairs.count { case Seq(l, r) => l.overlaps(r) }
}

@main def Day4() = CampCleanup.printAnswers()
