import scala.collection.mutable

object SupplyStacks extends AOCSolution[String, String](5) {
  private val blocks = inputBlocks()
  private val crates = blocks.head.dropRight(1).map(_.grouped(4).map(_(1)).toList).transpose.map(_.filterNot(_.isWhitespace).toList).toList
  private val instructions = blocks.last.map(i => i.split(' ').filter(_.forall(_.isDigit)).map(_.toInt).toList).toList

  private def move(crates: List[List[Char]], count: Int, from: Int, to: Int) = {
    val cratesBuffer = crates.toBuffer
    val movingCrates = cratesBuffer(from - 1).take(count)
    cratesBuffer(from - 1) = cratesBuffer(from - 1).drop(count)
    cratesBuffer(to - 1) = movingCrates ++ cratesBuffer(to - 1)
    cratesBuffer.toList
  }

  private def applyInstructions(crates: List[List[Char]], instructions: List[List[Int]]) =
    instructions.foldLeft(crates) { case (newCrates, List(count, from, to)) => move(newCrates, count, from, to) }

  protected val FirstPartAnswer = applyInstructions(crates, instructions.flatMap { case List(count, from, to) => List.fill(count)(List(1, from ,to)) }).map(_.head).mkString
  protected val SecondPartAnswer = applyInstructions(crates, instructions).map(_.head).mkString
}

@main def Day5() = SupplyStacks.printAnswers()
