package day04

import utils.Base

case class Section(definition: String):
  val Array(start: Int, end: Int) = definition.split("-").map(_.toInt)

  def isFullyContainedIn(other: Section): Boolean =
    start >= other.start && end <= other.end

  def overlapsRight(other: Section): Boolean =
    other.start <= start && other.end >= start

  def overlapsLeft(other: Section): Boolean =
    start <= other.start && end >= other.start

  def overlapsWith(other: Section): Boolean =
    isFullyContainedIn(other) || overlapsLeft(other) || overlapsRight(other)

object Cleanup extends Base:
  def dirName: String = "day04"
  def isTest: Boolean = false

  def sections: Seq[(Section, Section)] = input.parseCSV.map {
    case Seq(first, second) => (Section(first), Section(second))
  }

  override def part1: Int = sections.count { case (s1, s2) =>
    s1.isFullyContainedIn(s2) || s2.isFullyContainedIn(s1)
  }

  override def part2: Int = sections.count { case (s1, s2) =>
    s1.overlapsWith(s2)
  }

  @main def main = run()
