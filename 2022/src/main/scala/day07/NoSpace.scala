package day07

import utils.{Base, InputSource}

object DirContent:
  def fromString(currDir: String)(s: String): DirContent =
    if s.take(3) == "dir" then
      val name = s.drop(4)
      Dir(s"$currDir/$name", currDir)
    else
      val size = s.takeWhile(_.isDigit)
      val name = s.drop(size.length + 1)
      File(s"$currDir/$name", size.toInt, currDir)

trait DirContent:
  val name: String
  val parent: String

case class File(name: String, size: Int, parent: String) extends DirContent

case class Dir(name: String, parent: String) extends DirContent

object FileSystem:
  def empty: FileSystem = new FileSystem(
    Map.empty[String, Seq[String]],
    Map("/" -> Dir("/", "/"))
  )

case class FileSystem(
    structure: Map[String, Seq[String]],
    data: Map[String, DirContent]
):
  private val diskSpace = 70000000
  private val requiredSpace = 30000000

  private lazy val freeSpace = diskSpace - getSize("/")
  lazy val missingSpace: Int = requiredSpace - freeSpace

  def addContent(currDir: Dir, content: Seq[DirContent]): FileSystem =
    val contentNames = content.map(_.name)

    val newStructure = structure.updatedWith(currDir.name) {
      case Some(list) => Some(contentNames ++ list)
      case None       => Some(contentNames)
    }

    val newData = content.foldLeft(data) { case (data, c) =>
      data + (c.name -> c)
    }

    FileSystem(newStructure, newData)

  def getDir(name: String): Dir = data(name) match
    case dir: Dir => dir
    case file: File =>
      println(file.name)
      throw Exception()

  private def getSize(name: String): Int = data(name) match
    case dir: Dir   => structure(dir.name).map(getSize).sum
    case file: File => file.size

  def getSize(content: DirContent): Int = getSize(content.name)

  def dirs: Seq[Dir] = data.values.collect { case dir: Dir =>
    dir
  }.toSeq

object Terminal:
  def apply(commands: Seq[String]): Terminal =
    val fileSystem = FileSystem.empty
    new Terminal(commands, fileSystem.getDir("/"), fileSystem)

case class Terminal(
    commands: Seq[String],
    currDir: Dir,
    fileSystem: FileSystem
):
  private def executeNextCommand: Terminal = commands.head match
    case "$ ls" =>
      val (content, nextCommands) = commands.tail.span(_.head != '$')
      val newStructure = fileSystem.addContent(
        currDir,
        content.map(DirContent.fromString(currDir.name))
      )

      Terminal(nextCommands, currDir, newStructure)
    case cd =>
      val dir = cd.drop(5) match
        case ".."    => currDir.parent
        case "/"     => "/"
        case dirName => s"${currDir.name}/$dirName"
      moveInto(fileSystem.getDir(dir))

  private def moveInto(dir: Dir): Terminal =
    Terminal(commands.tail, dir, fileSystem)

  def buildFileSystem: FileSystem =
    if commands.isEmpty then fileSystem else executeNextCommand.buildFileSystem

object NoSpace extends Base:

  private val commandList: Seq[String] = input.toLines
  private val fileSystem = Terminal(commandList).buildFileSystem
  private val dirSizes = fileSystem.dirs.map(fileSystem.getSize)

  override def part1: Any = dirSizes.filter(_ <= 100000).sum

  override def part2: Any =
    dirSizes.filter(_ >= fileSystem.missingSpace).min

  @main def main(): Unit = run()
