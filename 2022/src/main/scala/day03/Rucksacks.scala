package day03

import utils.{Base, InputSource}

case class Item(identifier: Char):
  private def intValue: Int = identifier.toInt
  def priority: Int =
    if identifier.isLower then intValue - 96 else intValue - 38

case class Rucksack(items: Seq[Item]):
  private def compartments: (Seq[Item], Seq[Item]) = items.splitAt(items.length / 2)

  def commonItem: Item =
    val (comp1, comp2) = compartments
    comp1.toSet.intersect(comp2.toSet).head

object Rucksacks extends Base:

  private def rucksacks: Seq[Rucksack] =
    input.toLines.map(s => Rucksack(s.map(Item.apply)))

  override def part1: Int = rucksacks.map(_.commonItem.priority).sum

  override def part2: Int = rucksacks
    .grouped(3)
    .map(group =>
      group
        .map(_.items.toSet)
        .reduce(_.intersect(_))
        .head
        .priority
    )
    .sum

  @main def main(): Unit = run()
