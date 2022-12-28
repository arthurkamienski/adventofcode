package day11

case class Game(monkeyTurn: MonkeyNumber, monkeys: Map[MonkeyNumber, Monkey]):
  val currentMonkey: Monkey = monkeys(monkeyTurn)

  def nextMonkey: MonkeyNumber = (monkeyTurn + 1) % monkeys.size

  def updatedMonkeys: Map[MonkeyNumber, Monkey] =
    currentMonkey.thrownItems.foldLeft(monkeys) {
      case (monkeys, (number, item)) =>
        monkeys + (number -> monkeys(number).grabItem(item))
    } + (monkeyTurn -> currentMonkey.takeTurn)

  def playTurn: Game = Game(nextMonkey, updatedMonkeys)

  def playRound: Game =
    if monkeyTurn == monkeys.size - 1 then playTurn else playTurn.playRound

  def playRounds(n: Int): Game = (1 to n)
    .foldLeft(this) { case (game, _) =>
      game.playRound
    }

  def monkeyBusiness: Long =
    monkeys.values.toSeq.map(_.throws.toLong).sorted.reverse.take(2).product

  def orderedMonkeys: Seq[Monkey] = monkeys.values.toSeq.sortBy(_.number)
