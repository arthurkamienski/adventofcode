package day09

import utils.Base

case class Knot(x: Int, y: Int):
  def move(dir: Char): Knot = dir match
    case 'U' => Knot(x, y + 1)
    case 'D' => Knot(x, y - 1)
    case 'L' => Knot(x - 1, y)
    case 'R' => Knot(x + 1, y)

  def follow(next: Knot): Knot =
    val distX = next.x - x
    val distY = next.y - y
    val isTouching = distX.abs <= 1 && distY.abs <= 1

    if isTouching then this else Knot(x + distX.sign, y + distY.sign)

object Rope:
  def apply(n: Int): Rope =
    new Rope((1 to n).map(_ => Knot(0, 0)), Seq.empty[Knot])

case class Rope(knots: Seq[Knot], prevTails: Seq[Knot]):
  val head: Knot = knots.head
  val tail: Seq[Knot] = knots.tail
  val last: Knot = knots.last

  def move(dir: Char): Rope =
    val newHead = head.move(dir)

    val newKnots = knots.tail.scanLeft(newHead) { case (prev, next) =>
      next.follow(prev)
    }

    val newLast = newKnots.last

    val newPrevTails = if newLast == last then prevTails else last +: prevTails

    Rope(newKnots, newPrevTails)

  def move(moves: Seq[Char]): Rope = moves.foldLeft(this)(_.move(_))

  def tailPositions: Int = (tail +: prevTails).toSet.size

object RopeBridge extends Base:
  def dirName: String = "day09"
  def isTest: Boolean = false

  val moves: Seq[Char] = input.toLines.flatMap { line =>
    val Array(char, n) = line.split(" ")
    (1 to n.toInt).map(_ => char.head)
  }

  override def part1: Any = Rope(2).move(moves).tailPositions
  override def part2: Any = Rope(10).move(moves).tailPositions

  @main def main = run()
