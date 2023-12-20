package day21

import utils.Base

import scala.annotation.tailrec

case class Monkey(
    id: String,
    operation: String,
    value: Option[Double],
    dependsOn: Set[String]
):
  def getValue(monkeys: Map[String, Monkey]): Monkey = value match
    case None =>
      val dependsOnMonkeys = dependsOn.map(monkeys(_)).toSeq

      if dependsOnMonkeys.forall(_.value.isDefined) then
        val dependsOnValues = dependsOnMonkeys
          .map(_.value.get)

        val newValue = dependsOnValues
          .reduce(operation match
            case "+" => _ + _
            case "*" => _ * _
            case "/" => _ / _
            case "-" => _ - _
          )
        copy(value = Some(newValue))
      else this
    case _ => this

object MonkeyMath extends Base:
  val monkeys: Map[String, Monkey] = input.toLines.map { s =>
    val NumMonkey = """(\w+): (\d+)""".r
    val OpMonkey = """(\w+): (\w+) ([\-/+*]) (\w+)""".r

    s match
      case NumMonkey(id, value) =>
        id -> Monkey(id, "", Some(value.toInt), Set.empty)
      case OpMonkey(id, m1, operation, m2) =>
        id -> Monkey(id, operation, None, Set(m1, m2))
      case _ => throw RuntimeException(s"Invalid monkey: $s")
  }.toMap

  val monkeysWithNewRoot = monkeys.updated(
    "root",
    monkeys("root").copy(operation = "-")
  )

  @tailrec
  private def findValues(monkeys: Map[String, Monkey]): Map[String, Monkey] =
    val newMonkeys = monkeys.keys.foldLeft(monkeys) { case (acc, id) =>
      acc.updated(id, acc(id).getValue(acc))
    }
    if newMonkeys("root").value.isDefined then newMonkeys
    else findValues(newMonkeys)

  def getRootValueGivenHumanValue(humnValue: Double): Double =
    findValues(
      monkeysWithNewRoot.updated(
        "humn",
        monkeysWithNewRoot("humn").copy(value = Some(humnValue))
      )
    )("root").value.get

  // Note: I CoPiloted this very hard
  def findRoot(f: Double => Double): Double =
    val epsilon = 0.1
    val maxIterations = 10000
    val max = 10000000000000.0
    val min = -10000000000000.0

    val isAscending = f(min) < f(max)

    @tailrec
    def findRootRec(
        f: Double => Double,
        min: Double,
        max: Double,
        iterations: Int,
        isAcending: Boolean
    ): Double =
      val mid = (max + min) / 2
      val fmid = f(mid)

      val newMin = if fmid > 0 ^ isAscending then mid else min
      val newMax = if fmid > 0 ^ isAscending then max else mid

      if iterations > maxIterations then mid
      else if (max - min) < epsilon then mid
      else findRootRec(f, newMin, newMax, iterations + 1, isAcending)
    findRootRec(f, min, max, 0, isAscending)

  override def part1: Any = BigDecimal(findValues(monkeys)("root").value.get)
  override def part2: Any = BigDecimal(findRoot(getRootValueGivenHumanValue).round)

  @main def main(): Unit = run()
