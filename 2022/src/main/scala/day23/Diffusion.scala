package day23

import utils.Base

import scala.annotation.tailrec

case class Direction(dx: Int, dy: Int)
object Direction:
  val N: Direction = Direction(0, -1)
  val S: Direction = Direction(0, 1)
  val W: Direction = Direction(-1, 0)
  val E: Direction = Direction(1, 0)

case class Elf(x: Int, y: Int):
  private val moveOrder: Seq[Direction] =
    Seq(Direction.N, Direction.S, Direction.W, Direction.E)

  lazy val neighbors: Set[Elf] = (-1 to 1).flatMap { dy =>
    (-1 to 1).map { dx =>
      Elf(x + dx, y + dy)
    }
  }.toSet - this

  private def getProposedMove(dir: Direction, otherElves: Set[Elf]): Option[Elf] =
    val Direction(dx, dy) = dir
    val directionNeighbors =
      if dx == 0 then neighbors.filter(_.y == (y + dy))
      else neighbors.filter(_.x == (x + dx))

    if directionNeighbors.intersect(otherElves).isEmpty then
      Some(Elf(x + dx, y + dy))
    else None

  def proposeMove(round: Int, otherElves: Set[Elf]): Option[Elf] =
    if neighbors.intersect(otherElves).isEmpty then None
    else
      (round to round + 3).flatMap { r =>
        getProposedMove(moveOrder(r % 4), otherElves)
      }.headOption

object Diffusion extends Base:
  private val elves: Set[Elf] = input.toLines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.flatMap { (char, x) =>
      if char == '#' then Some(Elf(x, y)) else None
    }
  }.toSet

  private def getProposedMoves(round: Int, elves: Set[Elf]): Map[Elf, Set[Elf]] =
    elves.foldLeft(Map.empty[Elf, Set[Elf]]) { case (moves, elf) =>
      elf.proposeMove(round, elves - elf) match
        case Some(newElf) =>
          moves.updated(newElf, moves.getOrElse(newElf, Set.empty) + elf)
        case None => moves
    }

  private def moveElves(elves: Set[Elf], proposedMoves: Map[Elf, Set[Elf]]): Set[Elf] =
    val movedElves = proposedMoves.filter(_._2.size == 1).map {
      case (elf, elves) => elves.head -> elf
    }

    val notMovedElves = elves -- movedElves.keys

    movedElves.values.toSet ++ notMovedElves

  def printElves(elves: Set[Elf]): Unit =
    val minX = elves.map(_.x).min
    val maxX = elves.map(_.x).max
    val minY = elves.map(_.y).min
    val maxY = elves.map(_.y).max

    (minY to maxY).foreach { y =>
      (minX to maxX).foreach { x =>
        if elves.contains(Elf(x, y)) then print('#')
        else print('.')
      }
      println()
    }

  @tailrec
  private def runRounds(round: Int, maxRounds: Int, elves: Set[Elf]): Set[Elf] =
    val proposedMoves = getProposedMoves(round, elves)
    val movedElves = moveElves(elves, proposedMoves)
    if movedElves == elves || round == maxRounds then elves
    else runRounds(round + 1, maxRounds, movedElves)

  private def runRounds(maxRounds: Int): Set[Elf] =
    runRounds(0, maxRounds, elves)

  private def firstRoundWithNoMoves: Int =
    @tailrec
    def loop(round: Int, elves: Set[Elf]): Int =
      val proposedMoves = getProposedMoves(round, elves)
      val movedElves = moveElves(elves, proposedMoves)
      if movedElves == elves then round
      else loop(round + 1, movedElves)

    loop(0, elves) + 1

  private def emptyTiles(elves: Set[Elf]): Int =
    val minX = elves.map(_.x).min
    val maxX = elves.map(_.x).max
    val minY = elves.map(_.y).min
    val maxY = elves.map(_.y).max

    (maxX - minX + 1) * (maxY - minY + 1) - elves.size

  override def part1: Any = emptyTiles(runRounds(10))
  override def part2: Any = firstRoundWithNoMoves

  @main def main(): Unit = run()
