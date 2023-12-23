package day22

import utils.Base

import scala.annotation.tailrec

case class CubeMap(
    quadrants: Map[(Int, Int), MapQuadrant],
    sideLength: Int,
    quadrantConnections: Map[
      (Int, Int),
      Map[Direction, ((Int, Int), Direction)]
    ]
) extends BaseMap:
  override def getNextQuadrant(
      quadrant: MapQuadrant,
      direction: Direction
  ): MapQuadrant = quadrants(
    quadrantConnections(quadrant.coord)(direction)._1
  )

  override def translatePosition(
      pos: Position,
      currQuadrant: MapQuadrant,
      newQuadrant: MapQuadrant
  ): Position =
    val Position(x, y, dir) = currQuadrant.toRelativePosition(pos)

    val newDir = quadrantConnections(currQuadrant.coord)(dir)._2

    val (newX, newY) =
      if newDir == dir then (x, y)
      else if newDir.x == -dir.x && newDir.y == -dir.y then
        (sideLength - 1 - x, sideLength - 1 - y)
      else if newDir.x == -dir.y && newDir.y == dir.x then
        (sideLength - 1 - y, x)
      else if newDir.x == dir.y && newDir.y == -dir.x then
        (y, sideLength - 1 - x)
      else throw new Exception("Invalid direction")

    val newPos = newDir match
      case Direction.Up =>
        Position(sideLength - 1, newY, Direction.Up)
      case Direction.Down =>
        Position(0, newY, Direction.Down)
      case Direction.Left =>
        Position(newX, sideLength - 1, Direction.Left)
      case Direction.Right =>
        Position(newX, 0, Direction.Right)

    newQuadrant.toAbsolutePosition(newPos)
