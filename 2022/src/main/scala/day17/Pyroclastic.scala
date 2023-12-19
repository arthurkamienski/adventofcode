package day17

import utils.Base

case class Coord(x: Int, y: Int):
  def add(deltaX: Int, deltaY: Int): Coord = Coord(x + deltaX, y + deltaY)

object Pyroclastic extends Base:

  val jets: Iterator[Int] = LazyList
    .continually(input.trim.map {
      case '>' => 1
      case '<' => -1
    })
    .flatten
    .iterator

  val shapes: Iterator[Coord => Shape] =
    LazyList
      .continually(
        Seq(HBar.apply, Cross.apply, MirroredL.apply, VBar.apply, Cube.apply)
      )
      .flatten
      .iterator

  private lazy val repeatingChamber: Chamber =
    (1 to 4000).foldLeft(Chamber(shapes, jets)) { case (chamber, _) =>
      chamber.dropNewShapeToBottom
    }

  override def part1: Any =
    repeatingChamber.heightAtNShapes(2022).get

  override def part2: Any =
    repeatingChamber.heightAtNShapes(BigInt("1000000000000")).get

  @main def main(): Unit = run()
