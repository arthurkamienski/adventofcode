package day22

import utils.Base

import scala.annotation.tailrec

case class Instruction(distance: Int, rotation: Option[Char])

case class Direction(x: Int, y: Int):
  def rotate(rotation: Option[Char]): Direction =
    rotation match
      case Some('R') => Direction(y, -x)
      case Some('L') => Direction(-y, x)
      case _         => this

  def value: Int = (x, y) match
    case (0, 1)   => 0
    case (1, 0)   => 1
    case (0, -1)  => 2
    case (-1, 0)  => 3
    case _        => throw new Exception("Invalid direction")

case class Position(x: Int, y: Int, dir: Direction):
  def rotate(rotation: Option[Char]): Position =
    Position(x, y, dir.rotate(rotation))

  def finalPassword: Int = (x + 1) * 1000 + (y + 1) * 4 + dir.value

object MonkeyMap extends Base:
  val trueInput = input // to make it easier to change to test

  val mapInput = trueInput.split("\n\n").head
  val directionsInput = trueInput.split("\n\n").last

  val sideLength = if mapInput.toLines.length % 50 == 0 then 50 else 4

  val quadrants =
    mapInput
      .split("\n\n")
      .head
      .toLines
      .grouped(sideLength)
      .zipWithIndex
      .foldLeft(Map.empty[(Int, Int), Seq[String]]) {
        case (acc, (lines, row)) =>
          lines.foldLeft(acc) { case (acc, line) =>
            line.grouped(sideLength).zipWithIndex.foldLeft(acc) {
              case (acc, (chunk, col)) =>
                acc.updated(
                  (row, col),
                  acc.getOrElse((row, col), Seq.empty) :+ chunk
                )
            }
          }
      }
      .filter(_._2.head.forall(_ != ' '))
      .map { case (coord, lines) =>
        val topLeft = (coord._1 * sideLength, coord._2 * sideLength)
        val walls = lines.zipWithIndex.flatMap { case (line, row) =>
          line.zipWithIndex.flatMap { case (char, col) =>
            if char == '#' then Some((row, col))
            else None
          }
        }

        coord -> MapQuadrant(coord, topLeft, sideLength, walls.toSet)
      }

  val path: Seq[Instruction] = {
    val line = trueInput.split("\n\n").last

    """(\d+[RL]?)""".r
      .findAllIn(line)
      .map(s =>
        if Set('R', 'L').contains(s.last) then
          Instruction(s.init.toInt, Some(s.last))
        else Instruction(s.toInt, None)
      )
      .toSeq
  }

  val startPos =
    val (x, y) = quadrants.toSeq.minBy(_._1)._2.topLeft
    Position(x, y, Direction(0, 1))

  val board = BoardMap(quadrants, sideLength)

  override def part1: Any =
    path.foldLeft(startPos) { case (pos, instruction) =>
      board.nextPos(pos, instruction)
    }.finalPassword

  override def part2: Any = 0

  @main def main(): Unit = run()
