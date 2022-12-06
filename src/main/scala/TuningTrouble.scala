object TuningTrouble extends AOCSolution[Int, Int](6) {
  private def `.`(n: Int) = inputString.sliding(n).indexWhere(_.distinct.length == n) + n

  protected val FirstPartAnswer: Int = `.`(4)
  protected val SecondPartAnswer: Int = `.`(14)
}

@main def Day6() = TuningTrouble.printAnswers()
