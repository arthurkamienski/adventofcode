package day11

import utils.{Base, InputSource}

type MonkeyNumber = Int

object MonkeyInTheMiddle extends Base:

  private def notes: Seq[Seq[String]] = input.toChunks.map(_.toLines)

  private def createMonkeys(bored: Boolean): Map[MonkeyNumber, Monkey] =
    val monkeys = notes
      .map(Monkey.fromNotes(bored))

    val mods = monkeys.map(_.chooseMonkeyToThrow.mod)

    monkeys
      .map(_.addMods(mods))
      .map(monkey => monkey.number -> monkey)
      .toMap

  private def boredMonkeys: Map[MonkeyNumber, Monkey] = createMonkeys(true)
  private def monkeys: Map[MonkeyNumber, Monkey] = createMonkeys(false)

  override def part1: Any = Game(0, boredMonkeys).playRounds(20).monkeyBusiness

  override def part2: Any = Game(0, monkeys).playRounds(10000).monkeyBusiness

  @main def main(): Unit = run()
