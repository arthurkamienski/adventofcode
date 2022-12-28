package day11

import utils.{Base, InputSource}

type MonkeyNumber = Int

object MonkeyInTheMiddle extends Base:
  override def inputSource: InputSource = InputSource("day11", isTest = false)

  def notes: Seq[Seq[String]] = input.toChunks.map(_.toLines)

  def createMonkeys(bored: Boolean): Map[MonkeyNumber, Monkey] =
    val monkeys = notes
      .map(Monkey.fromNotes(bored))

    val mods = monkeys.map(_.chooseMonkeyToThrow.mod)

    monkeys
      .map(_.addMods(mods))
      .map(monkey => monkey.number -> monkey)
      .toMap

  def boredMonkeys: Map[MonkeyNumber, Monkey] = createMonkeys(true)
  def monkeys: Map[MonkeyNumber, Monkey] = createMonkeys(false)

  override def part1: Any = Game(0, boredMonkeys).playRounds(20).monkeyBusiness

  override def part2: Any = Game(0, monkeys).playRounds(10000).monkeyBusiness

  @main def main = run()
