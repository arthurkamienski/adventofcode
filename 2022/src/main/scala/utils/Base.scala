package utils

import scala.io.Source

case class InputSource(
    dirName: String,
    isTest: Boolean = false,
    testNumber: Int = 1
):
  private def projectDir: String = System.getProperty("user.dir")
  private def pwd: String = s"$projectDir/src/main/scala"
  private def baseDir: String = s"$pwd/$dirName"

  private def testInputName: String = if testNumber > 1 then
    s"$baseDir/test_input_$testNumber.txt"
  else s"$baseDir/test_input.txt"

  private def inputName: String = s"$baseDir/input.txt"

  def path: String =
    if isTest then testInputName
    else inputName

  def read: String =
    val source = Source.fromFile(path)
    val lines = source.mkString

    source.close()

    lines

trait Base:
  def dirName: String = getClass.getCanonicalName.split("\\.").head

  def input: String = InputSource(dirName).read
  def testInput: String = InputSource(dirName, isTest = true).read

  def part1: Any
  def part2: Any

  def run(): Unit =
    println(part1)
    println(part2)

  extension (s: String)
    def toChunks: Seq[String] = s.split("\n\n")
    def toLines: Seq[String] = s.linesIterator.toSeq
    def toIntList: Seq[Int] = toLines.toIntList
    def parseCSV: Seq[Seq[String]] = toLines.map(_.split(","))

  extension (l: Seq[String]) def toIntList: Seq[Int] = l.map(_.toInt)
