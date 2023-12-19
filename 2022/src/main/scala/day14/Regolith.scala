package day14

import utils.{Base, InputSource}

import scala.annotation.tailrec

case class Point(x: Int, y: Int):
  def pointsBetween(other: Point): Seq[Point] =
    val deltaX = other.x - x
    val deltaY = other.y - y

    if deltaX == 0 then
      (0 to deltaY.abs).map { i =>
        Point(x, y + i * deltaY.sign)
      }
    else
      (0 to deltaX.abs).map { i =>
        Point(x + i * deltaX.sign, y)
      }

  def up: Point = Point(x, y - 1)
  def down: Point = Point(x, y + 1)
  def downLeft: Point = Point(x - 1, y + 1)
  def downRight: Point = Point(x + 1, y + 1)

object Regolith extends Base:

  private val rocks: Set[Point] = input.toLines.flatMap { path =>
    val points = path
      .split(" -> ")
      .map(_.split(",").map(_.toInt))
      .map(p => Point(p(0), p(1)))

    points.sliding(2).flatMap(ps => ps(0).pointsBetween(ps(1)))
  }.toSet

  private val initialRock: Point = rocks.filter(_.x == 500).minBy(_.y)
  private val lowestPoint: Int = rocks.map(_.y).max
  private val floorPosition: Int = lowestPoint + 2

  def printMap(sands: Set[Point]): Unit =
    val allPoints = sands ++ rocks
    val maxX = allPoints.map(_.x).max
    val minX = allPoints.map(_.x).min

    (0 to floorPosition).foreach { y =>
      (minX to maxX).foreach { x =>
        if rocks.contains(Point(x, y)) || y == floorPosition then print("#")
        else if sands.contains(Point(x, y)) then print("O")
        else print(".")
      }
      println()
    }

  @tailrec
  private def dropUntilFull(
      hasFloor: Boolean
  )(topSand: Point, occupied: Set[Point], sands: Set[Point]): Set[Point] =
    @tailrec
    def dropSand(position: Point, occupied: Set[Point]): Point =
      if !hasFloor && position.y > lowestPoint then position
      else if hasFloor && position.y == floorPosition - 1 then position
      else if !occupied.contains(position.down) then
        dropSand(position.down, occupied)
      else if !occupied.contains(position.downLeft) then
        dropSand(position.downLeft, occupied)
      else if !occupied.contains(position.downRight) then
        dropSand(position.downRight, occupied)
      else position

    val nextSand = dropSand(topSand.up, occupied)

    val newTopSand = if nextSand.y < topSand.y then nextSand else topSand

    if !hasFloor && nextSand.y > lowestPoint then sands
    else if hasFloor && nextSand.y == 0 then sands + nextSand
    else
      dropUntilFull(hasFloor)(newTopSand, occupied + nextSand, sands + nextSand)

  override def part1: Any =
    val sands = dropUntilFull(false)(initialRock, rocks, Set())
    sands.size

  override def part2: Any =
    val sands = dropUntilFull(true)(initialRock, rocks, Set())
    sands.size

  @main def main(): Unit = run()
