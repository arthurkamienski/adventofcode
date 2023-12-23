package day22

import utils.Base

import scala.annotation.tailrec

case class BoardMap(quadrants: Map[(Int, Int), MapQuadrant], sideLength: Int):
  val rowLimits = quadrants.keys
    .groupBy(_._2)
    .map { case (col, coords) =>
      val xs = coords.map(_._1)
      col -> (xs.min, xs.max)
    }

  val colLimits = quadrants.keys
    .groupBy(_._1)
    .map { case (row, coords) =>
      val ys = coords.map(_._2)
      row -> (ys.min, ys.max)
    }

  def isMovingOffEdge(position: Position): Boolean =
    val Position(x, y, Direction(xDir, yDir)) = position

    Set(-1, sideLength).intersect(Set(x + xDir, y + yDir)).nonEmpty

  def getWrapAroundCoord(coord: Int, dir: Int, limits: (Int, Int)): Int =
    val (min, max) = limits
    val newCoord = coord + dir

    if newCoord > max then min
    else if newCoord < min then max
    else newCoord

  def getNextQuadrant(
      quadrant: MapQuadrant,
      direction: Direction
  ): MapQuadrant =
    val (x, y) = quadrant.coord
    val Direction(xDir, yDir) = direction

    val newQuadrantX =
      getWrapAroundCoord(x, xDir, rowLimits(y))
    val newQuadrantY =
      getWrapAroundCoord(y, yDir, colLimits(x))

    quadrants(newQuadrantX, newQuadrantY)

  def getQuadrant(position: Position): MapQuadrant =
    val Position(x, y, _) = position
    quadrants((x / sideLength, y / sideLength))

  def translatePosition(
      pos: Position,
      currQuadrant: MapQuadrant,
      newQuadrant: MapQuadrant
  ): Position =
    val Position(x, y, dir @ Direction(xDir, yDir)) =
      currQuadrant.toRelativePosition(pos)
    val newX = getWrapAroundCoord(x, xDir, (0, sideLength - 1))
    val newY = getWrapAroundCoord(y, yDir, (0, sideLength - 1))

    newQuadrant.toAbsolutePosition(Position(newX, newY, dir))

  def getQuadrant(
      position: Position,
      distance: Int
  ): (MapQuadrant, Position, Int) =
    val quadrant = getQuadrant(position)

    if isMovingOffEdge(quadrant.toRelativePosition(position)) then
      val nextQuadrant = getNextQuadrant(quadrant, position.dir)
      val newPosition = translatePosition(position, quadrant, nextQuadrant)

      if nextQuadrant.isWall(newPosition) then (quadrant, position, 0)
      else (nextQuadrant, newPosition, distance - 1)
    else (quadrant, position, distance)

  @tailrec
  private def move(
      position: Position,
      distance: Int
  ): Position =
    if distance == 0 then position
    else
      val (quadrant, newPos, dist) = getQuadrant(position, distance)
      val (posAfterMove, remainingDist) = quadrant.nextPos(newPos, dist)

      move(posAfterMove, remainingDist)

  def nextPos(position: Position, instruction: Instruction): Position =
    val Position(x, y, _) = position
    val Instruction(distance, rotation) = instruction

    val newPos = move(position, distance)

    newPos.rotate(rotation)
