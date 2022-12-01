import scala.reflect.ClassTag

abstract class AOCSolution[FPT, SPT](day: Integer) {
  protected lazy val inputSource = io.Source.fromFile(s"inputs/day$day.txt")
  protected lazy val inputString = inputSource.mkString
  protected def inputLines[T](mapper: String => T = identity[String]) = inputSource.getLines().map(mapper).toSeq
  protected def inputBlocks[T:ClassTag](mapper: String => T = identity[String]) = inputString.split("\r\n\r\n").toSeq.map(_.split("\r\n").map(mapper).toSeq)

  protected val FirstPartAnswer: FPT
  protected val SecondPartAnswer: FPT

  def printAnswers() = {
    println(s"Day $day answers:")
    println(s"Part 1: $FirstPartAnswer")
    println(s"Part 2: $SecondPartAnswer")
  }
}
