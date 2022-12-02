object RockPaperScissors extends AOCSolution[Int, Int](2){
  private val VALUE = Seq(
    'A' -> 1, 'B' -> 2, 'C' -> 3,
    'X' -> 1, 'Y' -> 2, 'Z' -> 3
  ).toMap

  private def countScore(opponent: Int, me: Int) = ((me - opponent + 4) % 3) * 3 + me
  private def countMe(opponent: Int, outcome: Int) = (opponent + outcome) % 3 + 1

  private val pairs = inputLines(s => VALUE(s.head) -> VALUE(s.last))

  protected val FirstPartAnswer = pairs.map(countScore.tupled).sum
  protected val SecondPartAnswer = pairs.map(_._1)
    .zip(pairs.map(countMe.tupled))
    .map(countScore.tupled)
    .sum
}

@main def Day2() = RockPaperScissors.printAnswers()
