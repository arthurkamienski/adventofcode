package day11

case class Item(
    initValue: Int,
    worryLevel: Map[Int, Int] = Map.empty[Int, Int],
    calculateValue: Boolean = false
):
  def %(mod: Int): Int = worryLevel.getOrElse(mod, initValue % mod)

  def addMods(mods: Seq[Int]): Item =
    Item(
      initValue,
      mods.map(mod => mod -> (initValue % mod)).toMap,
      calculateValue = calculateValue
    )

  def modOp(
      op: (Int, Int) => Int
  )(other: Item): Item = Item(
    initValue,
    worryLevel.map { case (mod, rem) =>
      mod -> (op(rem, (other % mod)) % mod)
    }.toMap,
    calculateValue
  )

  def simpleOp(
      op: (Int, Int) => Int
  )(other: Item): Item = Item(
    op(initValue, other.initValue),
    worryLevel.map { case (mod, rem) =>
      mod -> (op(initValue, other.initValue) % mod)
    }.toMap,
    calculateValue
  )

  def * = if calculateValue then simpleOp(_ * _) else modOp(_ * _)
  def + = if calculateValue then simpleOp(_ + _) else modOp(_ + _)

  def / = simpleOp(_ / _)
  def /(i: Int): Item =
    simpleOp(_ / _)(Item(i))
