package utils

import scala.io.Source

trait Base:
  val input: String =
    if isTest then readInput(testInputName)
    else readInput(inputName)

  def dirName: String

  def projectDir: String = System.getProperty("user.dir")
  def pwd: String = s"$projectDir/src/main/scala"

  def testInputName: String = s"$pwd/$dirName/test_input.txt"
  def inputName: String = s"$pwd/$dirName/input.txt"

  def isTest: Boolean

  def part1: Int
  def part2: Int

  def readInput(fileName: String): String =
    val source = Source.fromFile(fileName)
    val lines = source.mkString

    source.close()

    lines

  def run(): Unit =
    println(part1)
    println(part2)

  extension (s: String)
    def toChunks: Seq[String] = s.split("\n\n")
    def toLines: Seq[String] = s.linesIterator.toSeq
    def toIntList: Seq[Int] = toLines.toIntList

  extension (l: Seq[String]) def toIntList: Seq[Int] = l.map(_.toInt)
