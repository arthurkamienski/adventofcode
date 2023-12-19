package day15

import utils.{Base, InputSource}

case class Beacon(x: Int, y: Int, beaconX: Int, beaconY: Int):
  private val exclusionZone: Int = Math.abs(x - beaconX) + Math.abs(y - beaconY)
  def coverageArea(row: Int): Option[(Int, Int)] =
    val distanceToRow = Math.abs(row - y)
    if distanceToRow > exclusionZone then None
    else
      val coverage = Math.abs(exclusionZone - distanceToRow)
      Some((x - coverage, x + coverage))

  def boundedCoverageArea(minX: Int, maxX: Int)(row: Int): Option[(Int, Int)] =
    val coverage = coverageArea(row)
    coverage.map { case (start, end) =>
      (Math.max(minX, start), Math.min(maxX, end))
    }

object Beacons extends Base:

  private val beacons: Seq[Beacon] = input.toLines.map { line =>
    val Pattern =
      """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
    val Pattern(x, y, bx, by) = line: @unchecked

    Beacon(x.toInt, y.toInt, bx.toInt, by.toInt)
  }

  private def mergeCoverages(coverages: Seq[(Int, Int)]): Seq[(Int, Int)] =
    val sortedCoverages = coverages.sortBy(_._1)
    coverages.sortBy(_._1).foldLeft(Seq.empty[(Int, Int)]) { (acc, coverage) =>
      acc match
        case Nil => Seq(coverage)
        case last :: rest =>
          if last._2 >= coverage._1 then
            (last._1, Math.max(last._2, coverage._2)) +: rest
          else if last._2 + 1 == coverage._1 then (last._1, coverage._2) +: rest
          else coverage +: acc
    }

  override def part1: Any =
    val coverages = beacons.flatMap(_.coverageArea(2000000))
    mergeCoverages(coverages).map { case (start, end) => end - start }.sum
  override def part2: Any =
    val bound = 4000000
    val beaconPos = (0 to bound)
      .map(row =>
        row -> mergeCoverages(
          beacons.flatMap(_.boundedCoverageArea(0, bound)(row))
        )
      )
      .filter(_._2.length > 1)
      .map { case (row, (end, _) :: rest) =>
        (end - 1, row)
      }
      .head

    beaconPos._1.toLong * 4000000 + beaconPos._2

  @main def main(): Unit = run()
