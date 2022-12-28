package day11

case class Decider(
    mod: Int,
    monkeyTrue: MonkeyNumber,
    monkeyFalse: MonkeyNumber
) extends (Item => MonkeyNumber):
  def apply(item: Item): MonkeyNumber =
    if (item % mod) == 0 then monkeyTrue else monkeyFalse

case class Monkey(
    number: MonkeyNumber,
    items: Seq[Item],
    inspect: Item => Item,
    chooseMonkeyToThrow: Decider,
    bored: Boolean,
    throws: Int = 0
):
  def getBored(item: Item): Item = if bored then item / 3 else identity(item)

  lazy val thrownItems: Seq[(MonkeyNumber, Item)] = items.map { item =>
    val itemWithNewWorryLevel = getBored(inspect(item))
    val target = chooseMonkeyToThrow(itemWithNewWorryLevel)

    target -> itemWithNewWorryLevel
  }

  def takeTurn: Monkey = Monkey(
    number,
    Seq.empty[Item],
    inspect,
    chooseMonkeyToThrow,
    bored,
    throws + thrownItems.length
  )

  def withItems(items: Seq[Item]): Monkey =
    Monkey(number, items, inspect, chooseMonkeyToThrow, bored, throws)

  def grabItem(item: Item): Monkey = withItems(items :+ item)

  def addMods(mods: Seq[Int]): Monkey = Monkey(
    number = number,
    items = items.map(_.addMods(mods)),
    inspect = inspect,
    chooseMonkeyToThrow = chooseMonkeyToThrow,
    bored = bored,
    throws = throws
  )

object Monkey:
  private def parseOperation(definition: String): Item => Item =
    val Array(a, op, b) =
      definition.split(": ").last.split(" = ").last.split(" ")

    val stringToVal = (s: String) =>
      (x: Item) =>
        s match
          case "old" => x
          case _     => Item(s.toInt)

    val firstVal = stringToVal(a)
    val secondVal = stringToVal(b)

    val f = (a: Item, b: Item) =>
      op match
        case "+" => a + b
        case "*" => a * b

    (x => f(firstVal(x), secondVal(x)))

  private def parseTest(definition: Seq[String]): Decider =
    val Seq(test, condTrue, condFalse) = definition
    val radix = test.split(" ").last.toInt
    val monkeyTrue = condTrue.split(" ").last.toInt
    val monkeyFalse = condFalse.split(" ").last.toInt

    Decider(radix, monkeyTrue, monkeyFalse)

  def fromNotes(bored: Boolean)(notes: Seq[String]): Monkey =
    val number = notes.head.drop(7).init.toInt

    val startingItems =
      notes(1)
        .split(": ")
        .last
        .split(", ")
        .map(n => Item(n.toInt, calculateValue = bored))
    val op = parseOperation(notes(2))
    val test = parseTest(notes.drop(3))

    Monkey(number, startingItems, op, test, bored)
