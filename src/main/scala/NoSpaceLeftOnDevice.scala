import scala.annotation.tailrec

case class File(name: String, size: Long)
case class Folder(name: String, subfolders: List[Folder] = List.empty, files: List[File] = List.empty) {
  lazy val size: Long = subfolders.map(_.size).sum + files.map(_.size).sum
  def foldersSizes: List[Long] = size +: subfolders.flatMap(_.foldersSizes)
  override def toString: String = {
    val subfoldersPart = subfolders.map(_.toString.split("\n").map(f => s"    $f").mkString("\n")).mkString("\n") + (if (subfolders.isEmpty) "" else "\n")
    val filesPart = files.map(f => s"    $f").mkString("\n") + "\n"

    s"$name\n" + subfoldersPart + filesPart
  }
}

object NoSpaceLeftOnDevice extends AOCSolution[Long, Long](7) {
  private def parseFolder(current: Folder, searchOutput: List[List[String]]): (Folder, List[List[String]]) = {
    (0 to searchOutput.size).zip(searchOutput).foreach  {
      case (_, List("$", "ls")) => ()
      case (_, List("dir", _)) => ()

      case (i, List("$", "cd", "..")) => return (current, searchOutput.drop(i + 1))
      case (i, List("$", "cd", dir)) =>
        val (subfolder, tailOutput) = parseFolder(Folder(dir), searchOutput.drop(i + 1))
        val newCurrent = current.copy(subfolders = current.subfolders :+ subfolder)
        return parseFolder(newCurrent, tailOutput)
      case (i, List(size, name)) =>
        val newCurrent = current.copy(files = current.files :+ File(name, size.toInt))
        return parseFolder(newCurrent, searchOutput.drop(i + 1))

      case _ => ()
    }

    (current, List.empty)
  }

  private val fileSystem = parseFolder(Folder("root"), inputLines(_.split(' ').toList).toList)._1

  protected val FirstPartAnswer = fileSystem.foldersSizes.filter(_ <= 100000).sum
  protected val SecondPartAnswer = fileSystem.foldersSizes.filter(_ >= fileSystem.size - 40000000).min
}

@main def Day7() = NoSpaceLeftOnDevice.printAnswers()
