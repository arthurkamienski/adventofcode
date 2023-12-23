package day22

import utils.Base

import scala.annotation.tailrec

trait BaseMap:
  val quadrants: Map[(Int, Int), MapQuadrant]
  val sideLength: Int

  def getNextQuadrant(
      quadrant: MapQuadrant,
      direction: Direction
  ): MapQuadrant

  def translatePosition(
      pos: Position,
      currQuadrant: MapQuadrant,
      newQuadrant: MapQuadrant
  ): Position

  private def isMovingOffEdge(position: Position): Boolean =
    val Position(x, y, Direction(xDir, yDir)) = position

    Set(-1, sideLength).intersect(Set(x + xDir, y + yDir)).nonEmpty

  private def getQuadrant(position: Position): MapQuadrant =
    val Position(x, y, _) = position
    quadrants((x / sideLength, y / sideLength))

  private def getQuadrant(
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
