package day12

import utils.{Base, InputSource}

import scala.collection.mutable.PriorityQueue

case class Coord(i: Int, j: Int):
  def neighbors: Seq[Coord] = Seq(
    (i + 1, j),
    (i, j + 1),
    (i - 1, j),
    (i, j - 1)
  )

  def dist(other: Coord): Int = (i - other.i).abs + (j - other.j).abs

given Conversion[Tuple2[Int, Int], Coord] with
  def apply(c: Tuple2[Int, Int]): Coord = Coord(c._1, c._2)

case class GridPosition(
    representation: Char,
    coord: Coord
):
  def elevation: Int = representation match
    case 'S' => 'a'.toInt
    case 'E' => 'z'.toInt
    case _   => representation.toInt

  def hasPathTo(other: GridPosition): Boolean =
    (other.elevation - elevation) <= 1

  def dist(other: GridPosition): Int = coord.dist(other.coord)

implicit object IntTupleOrd extends Ordering[GridPosition]:
  def compare(a: GridPosition, b: GridPosition): Int =
    if a.elevation > b.elevation then 1
    else if b.elevation > a.elevation then -1
    else 0

object HeightMap:
  def apply(elevation: Seq[Seq[Char]]): HeightMap =
    val positions: Map[Coord, GridPosition] = elevation.zipWithIndex.flatMap {
      case (es, i) =>
        es.zipWithIndex.map { case (e, j) =>
          Coord(i, j) -> GridPosition(e, (i, j))
        }
    }.toMap

    val links = positions.values
      .map(p =>
        p -> p.coord.neighbors.flatMap(positions.get).filter(p.hasPathTo).toSeq
      )
      .toMap

    val start = positions.values.find(_.representation == 'S').get
    val end = positions.values.find(_.representation == 'E').get

    HeightMap(positions, links, start, end)

case class Path(
    priority: Int,
    steps: Int,
    currPos: GridPosition,
    tail: Seq[GridPosition]
)

case class HeightMap(
    positions: Map[Coord, GridPosition],
    links: Map[GridPosition, Seq[GridPosition]],
    start: GridPosition,
    end: GridPosition
):
  def bestPathFromStart: Path = bestPath(start)

  def possibleStartPoints: Seq[GridPosition] = links
    .filter { case (pos, neighbs) =>
      pos.representation == 'a' && neighbs.map(_.representation).contains('b')
    }
    .keys
    .toSeq

  def bestPathFromLowestElevation: Path =
    possibleStartPoints.map(bestPath).minBy(_.steps)

  def bestPath(from: GridPosition): Path =
    val queue = PriorityQueue(Path(from.dist(end), 0, from, Seq()))((a, b) =>
      if a.priority > b.priority then 1
      else if a.priority < b.priority then -1
      else 0
    )
    var visited = Set(from)

    while queue.head._3 != end do
      val path = queue.dequeue()

      visited = visited + path.currPos

      val neighbors =
        links(path.currPos)
          .filterNot(visited.contains)
          .map(p =>
            Path(
              -path.steps - 1 - p.dist(end),
              path.steps + 1,
              p,
              path.currPos +: path.tail
            )
          )

      queue.enqueue(neighbors: _*)

    queue.head

object HillClimb extends Base:
  override def inputSource: InputSource = InputSource("day12", isTest = false)

  val heightMap: HeightMap = HeightMap(input.toLines.map(_.toSeq))

  override def part1: Any = heightMap.bestPathFromStart.steps
  override def part2: Any = heightMap.bestPathFromLowestElevation.steps

  @main def main = run()
