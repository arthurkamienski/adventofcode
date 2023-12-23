package day22

import utils.Base

import scala.annotation.tailrec

case class MapQuadrant(
    coord: (Int, Int),
    topLeft: (Int, Int),
    sideLength: Int,
    walls: Set[(Int, Int)]
):
  val maxX: Int = topLeft._1 + sideLength - 1
  val maxY: Int = topLeft._2 + sideLength - 1

  def isWall(position: Position): Boolean =
    val Position(x, y, _) = toRelativePosition(position)
    walls.contains((x, y))

  val wallsInRow: Map[Int, Set[Int]] =
    walls
      .groupBy(_._1)
      .map { case (row, walls) =>
        row -> walls.map(_._2)
      }
      .withDefaultValue(Set.empty)

  val wallsInCol: Map[Int, Set[Int]] =
    walls
      .groupBy(_._2)
      .map { case (column, walls) =>
        column -> walls.map(_._1)
      }
      .withDefaultValue(Set.empty)

  def toRelativePosition(position: Position): Position =
    val Position(x, y, dir) = position
    Position(x - topLeft._1, y - topLeft._2, dir)

  def toAbsolutePosition(position: Position): Position =
    val Position(x, y, dir) = position
    Position(x + topLeft._1, y + topLeft._2, dir)

  def calculateFinalCoordinate(
      start: Int,
      dir: Int,
      distance: Int,
      walls: Set[Int]
  ): (Int, Int) =
    if dir == 0 then (start, 0)
    else
      val theoreticalFinal = start + dir * distance

      val wallInFront = dir match
        case -1 =>
          walls.filter(w => w < start && w >= theoreticalFinal).maxOption
        case 1 =>
          walls.filter(w => w > start && w <= theoreticalFinal).minOption
        case _ => None

      val posAfterWalls = wallInFront match
        case Some(wall) => wall - dir
        case None       => theoreticalFinal

      if posAfterWalls < 0 then (0, posAfterWalls.abs)
      else if posAfterWalls >= sideLength then
        (sideLength - 1, posAfterWalls - sideLength + 1)
      else (posAfterWalls, 0)

  def nextPos(position: Position, distance: Int): (Position, Int) =
    val Position(x, y, dir @ Direction(xDir, yDir)) = toRelativePosition(
      position
    )

    val (newX, remainingX) =
      calculateFinalCoordinate(x, xDir, distance, wallsInCol(y))
    val (newY, remainingY) =
      calculateFinalCoordinate(y, yDir, distance, wallsInRow(x))

    val newPos = Position(newX, newY, dir)

    (toAbsolutePosition(newPos), remainingX + remainingY)
