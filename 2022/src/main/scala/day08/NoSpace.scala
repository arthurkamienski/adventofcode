package day08

import utils.{Base, InputSource}

import scala.annotation.{tailrec, targetName}

enum Direction(val coord: Coord):
  case Top extends Direction((0, -1))
  case Bot extends Direction((0, 1))
  case Left extends Direction((-1, 0))
  case Right extends Direction((1, 0))

  private def reverse: Direction = Direction.coordToDirection(-this.coord)
  @targetName("negate")
  def unary_- : Direction = reverse

object Direction:
  private def coordToDirection(coord: Coord): Direction = coord match
    case Coord(0, -1) => Top
    case Coord(0, 1)  => Bot
    case Coord(-1, 0) => Left
    case Coord(1, 0)  => Right

import Direction._

given Conversion[(Int, Int), Coord] with
  def apply(c: (Int, Int)): Coord = Coord(c._1, c._2)

case class Coord(x: Int, y: Int):
  def moveTo(dir: Direction): Coord = this.+(dir.coord)
  @targetName("sum")
  def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  @targetName("negate")
  def unary_- : Coord = Coord(-x, -y)

type VisibilityGrid = Map[Coord, Boolean]
type ViewGrid = Map[Coord, Int]
type HeightGrid = Map[Coord, Int]

case class TreeGrid(heightGrid: HeightGrid):
  private val maxX = heightGrid.keys.maxBy(_.x).x
  private val maxY = heightGrid.keys.maxBy(_.y).y

  private def calculateScenicScore(
      coord: Coord,
      dir: Direction,
      viewGrid: ViewGrid
  ): ViewGrid =
    calculateScenicScore(coord, dir, viewGrid, (0 to 9).map(_ -> 0).toMap)

  @tailrec
  private def calculateScenicScore(
      coord: Coord,
      dir: Direction,
      viewGrid: ViewGrid,
      distFromHeight: Map[Int, Int]
  ): ViewGrid = heightGrid.get(coord) match
    case Some(treeHeight) =>
      val viewDistance =
        distFromHeight.view.filterKeys(_ >= treeHeight).values.min

      val newViewGrid = viewGrid.updatedWith(coord) {
        case Some(dirViewDistance) => Some(dirViewDistance * viewDistance)
        case None                  => Some(viewDistance)
      }

      val newDistFromHeight =
        (distFromHeight + (treeHeight -> 0)).view.mapValues(_ + 1).toMap

      calculateScenicScore(
        coord.moveTo(dir),
        dir,
        newViewGrid,
        newDistFromHeight
      )
    case None => viewGrid

  private def defineVisibility(
      coord: Coord,
      dir: Direction,
      visGrid: VisibilityGrid
  ): VisibilityGrid = defineVisibility(coord, dir, visGrid, -1)

  @tailrec
  private def defineVisibility(
      coord: Coord,
      dir: Direction,
      visGrid: VisibilityGrid,
      maxHeight: Int
  ): VisibilityGrid = heightGrid.get(coord) match
    case Some(treeHeight) =>
      val isHigher = treeHeight > maxHeight

      val nextCoord = coord.moveTo(dir)
      val nextMaxHeight = treeHeight.max(maxHeight)

      val newVisGrid = visGrid.updatedWith(coord) {
        case Some(bool) => Some(bool || isHigher)
        case None       => Some(isHigher)
      }

      defineVisibility(nextCoord, dir, newVisGrid, nextMaxHeight)
    case None => visGrid

  private def traverseGridInDirection[Grid](
      dir: Direction,
      maxCoord: Int,
      startCoord: Int => Coord,
      updateFunction: (Coord, Direction, Grid) => Grid
  )(grid: Grid): Grid =
    (0 to maxCoord).foldLeft(grid) { (prevGrid, c) =>
      updateFunction(startCoord(c), dir, prevGrid)
    }

  private def traverseGrid[Grid](
      updateFunction: (Coord, Direction, Grid) => Grid
  ): Grid => Grid = Seq(
    traverseGridInDirection(Bot, maxX, (_, 0), updateFunction),
    traverseGridInDirection(Top, maxX, (_, maxY), updateFunction),
    traverseGridInDirection(Right, maxY, (0, _), updateFunction),
    traverseGridInDirection(Left, maxY, (maxX, _), updateFunction)
  ).reduce(_.andThen(_))

  private val visibility: VisibilityGrid =
    traverseGrid(defineVisibility)(Map.empty[Coord, Boolean])

  private val scenicScores: ViewGrid =
    traverseGrid(calculateScenicScore)(Map.empty[Coord, Int])

  def visibleCoords: Int = visibility.values.count(identity)
  def highestScenicScore: Int = scenicScores.values.max

object TreeHouse extends Base:

  private val heightGrid: Map[Coord, Int] = input.toLines.zipWithIndex.flatMap {
    case (row, j) =>
      row.zipWithIndex.map { case (height, i) =>
        Coord(i, j) -> height.toString.toInt
      }
  }.toMap

  private val treeGrid = TreeGrid(heightGrid)

  override def part1: Any = treeGrid.visibleCoords
  override def part2: Any = treeGrid.highestScenicScore

  @main def main(): Unit = run()
