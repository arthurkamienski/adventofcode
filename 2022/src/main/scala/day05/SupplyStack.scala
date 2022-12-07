package day05

import utils.Base

type Crate = Char
type Stack = Seq[Crate]
type StackMap = Map[Int, Stack]

case class Move(definition: String):
  val Array(times: Int, from: Int, to: Int) =
    definition.split(" ").filter(_.matches("[0-9]+")).map(_.toInt)

  def move(moveFn: (Seq[Crate], Seq[Crate]) => Seq[Crate])(
      stacks: StackMap
  ): StackMap =
    val (taken, remaining) = stacks(from).splitAt(times)
    val newStack = moveFn(taken, stacks(to))

    stacks ++ Map(to -> newStack, from -> remaining)

  def moveOneAtATime(stacks: StackMap): StackMap =
    val moveFn = (taken: Seq[Crate], stack: Seq[Crate]) =>
      taken.reverse ++ stacks(to)
    move(moveFn)(stacks)

  def moveAllAtOnce(stacks: StackMap): StackMap =
    val moveFn = (taken: Seq[Crate], stack: Seq[Crate]) => taken ++ stacks(to)
    move(moveFn)(stacks)

object SupplyStack extends Base:
  def dirName: String = "day05"
  def isTest: Boolean = false

  val Array(cratesInput: String, movesInput: String) = input.split("\n\n")

  def stacks: StackMap =
    val crateLines = cratesInput.toLines.reverse
    val crateRows = crateLines.tail
      .map { row =>
        val getCrateFromString = (s: String) => s.filter(_.isUpper).headOption

        row
          .grouped(4)
          .map(getCrateFromString)
          .zipWithIndex
          .collect { case (Some(crate), i) =>
            (i, crate)
          }
      }

    crateRows.foldLeft(Map.empty[Int, Seq[Crate]]) { case (stackMap, crates) =>
      crates.foldLeft(stackMap) { case (prevStacks, (index, crate)) =>
        val oldStack = prevStacks.getOrElse(index + 1, Seq.empty[Crate])
        val updatedStack = crate +: oldStack

        prevStacks + (index + 1 -> updatedStack)
      }
    }

  def moves: Seq[Move] = movesInput.toLines.map(Move.apply)

  override def part1: Any = moves
    .foldLeft(stacks) { case (s, move) =>
      move.moveOneAtATime(s)
    }
    .toSeq
    .sorted
    .map(_._2.head)
    .mkString

  override def part2: Any = moves
    .foldLeft(stacks) { case (s, move) =>
      move.moveAllAtOnce(s)
    }
    .toSeq
    .sorted
    .map(_._2.head)
    .mkString

  @main def main = run()
