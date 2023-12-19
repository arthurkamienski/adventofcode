package day01

import utils.{Base, InputSource}


object Calories extends Base:

  private def caloriesPerElf: Seq[Int] = input.toChunks.map(_.toIntList.sum)

  override def part1: Int = caloriesPerElf.max

  override def part2: Int =
    val top3Elves = caloriesPerElf.sortBy(-_).take(3)
    top3Elves.sum

  @main def main(): Unit = run()
