object CalorieCounting extends AOCSolution[Int, Int](1) {
  private val calories = inputBlocks(_.toInt).map(_.sum).sorted

  protected val FirstPartAnswer = calories.last
  protected val SecondPartAnswer = calories.takeRight(3).sum
}

@main def main() = CalorieCounting.printAnswers()
