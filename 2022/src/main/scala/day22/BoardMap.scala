package day22

import utils.Base

import scala.annotation.tailrec

case class BoardMap(quadrants: Map[(Int, Int), MapQuadrant], sideLength: Int)
    extends BaseMap:
  private val rowLimits: Map[Int, (Int, Int)] = quadrants.keys
    .groupBy(_._2)
    .map { case (col, coords) =>
      val xs = coords.map(_._1)
      col -> (xs.min, xs.max)
    }

  private val colLimits: Map[Int, (Int, Int)] = quadrants.keys
    .groupBy(_._1)
    .map { case (row, coords) =>
      val ys = coords.map(_._2)
      row -> (ys.min, ys.max)
    }

  private def getWrapAroundCoord(
      coord: Int,
      dir: Int,
      limits: (Int, Int)
  ): Int =
    val (min, max) = limits
    val newCoord = coord + dir

    if newCoord > max then min
    else if newCoord < min then max
    else newCoord

  override def getNextQuadrant(
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

  override def translatePosition(
      pos: Position,
      currQuadrant: MapQuadrant,
      newQuadrant: MapQuadrant
  ): Position =
    val Position(x, y, dir @ Direction(xDir, yDir)) =
      currQuadrant.toRelativePosition(pos)
    val newX = getWrapAroundCoord(x, xDir, (0, sideLength - 1))
    val newY = getWrapAroundCoord(y, yDir, (0, sideLength - 1))

    newQuadrant.toAbsolutePosition(Position(newX, newY, dir))
