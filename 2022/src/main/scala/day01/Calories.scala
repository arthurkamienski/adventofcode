package day01

import utils.Base

object Calories extends Base:
  def dirName: String = "day01"
  def isTest: Boolean = false

  def caloriesPerElf: Seq[Int] = input.toChunks.map(_.toIntList.sum)

  override def part1: Int = caloriesPerElf.max

  override def part2: Int =
    val top3Elves = caloriesPerElf.sortBy(-_).take(3)
    top3Elves.sum

  @main def main = run()
