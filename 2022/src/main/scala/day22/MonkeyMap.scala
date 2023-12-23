package day22

import utils.Base

import scala.annotation.tailrec

case class Instruction(distance: Int, rotation: Option[Char])

object Direction:
  val Up: Direction = Direction(-1, 0)
  val Right: Direction = Direction(0, 1)
  val Down: Direction = Direction(1, 0)
  val Left: Direction = Direction(0, -1)

case class Direction(x: Int, y: Int):
  def rotate(rotation: Option[Char]): Direction =
    rotation match
      case Some('R') => Direction(y, -x)
      case Some('L') => Direction(-y, x)
      case _         => this

  def value: Int = (x, y) match
    case (0, 1)  => 0
    case (1, 0)  => 1
    case (0, -1) => 2
    case (-1, 0) => 3
    case _       => throw new Exception("Invalid direction")

case class Position(x: Int, y: Int, dir: Direction):
  def rotate(rotation: Option[Char]): Position =
    Position(x, y, dir.rotate(rotation))

  def finalPassword: Int = (x + 1) * 1000 + (y + 1) * 4 + dir.value

object MonkeyMap extends Base:
  val isTest = false

  private val trueInput: String = if isTest then testInput else input

  private val mapInput: String = trueInput.split("\n\n").head

  private val directionsInput: String = trueInput.split("\n\n").last

  val sideLength: Int = if isTest then 4 else 50

  val quadrants: Map[(Int, Int), MapQuadrant] =
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
    """(\d+[RL]?)""".r
      .findAllIn(directionsInput)
      .map(s =>
        if Set('R', 'L').contains(s.last) then
          Instruction(s.init.toInt, Some(s.last))
        else Instruction(s.toInt, None)
      )
      .toSeq
  }

  private val startPos: Position =
    val (x, y) = quadrants.toSeq.minBy(_._1)._2.topLeft
    Position(x, y, Direction.Right)

  private val board: BoardMap = BoardMap(quadrants, sideLength)

  private val quadrantConnections =
    if isTest then QuadrantConnections.testQuadrantConnections
    else QuadrantConnections.quadrantConnections

  val cube: CubeMap = CubeMap(quadrants, sideLength, quadrantConnections)

  override def part1: Any =
    path
      .foldLeft(startPos) { case (pos, instruction) =>
        board.nextPos(pos, instruction)
      }
      .finalPassword

  override def part2: Any =
    path
      .foldLeft(startPos) { case (pos, instruction) =>
        cube.nextPos(pos, instruction)
      }
      .finalPassword

  @main def main(): Unit = run()
