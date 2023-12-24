package day24

import utils.Base

import scala.annotation.tailrec

case class Blizzard(x: Int, y: Int, dir: (Int, Int)):
  val coord: (Int, Int) = (x, y)
  private val (dirX, dirY) = dir

  private def positionAtTime(t: Int): (Int, Int) =
    val newX =
      val thrNewX = (x + dirX * t) % Blizzard.maxX
      if thrNewX <= -1 then Blizzard.maxX + thrNewX % Blizzard.maxX
      else thrNewX

    val newY =
      val thrNewY = (y + dirY * t) % Blizzard.maxY
      if thrNewY <= -1 then Blizzard.maxY + thrNewY % Blizzard.maxY
      else thrNewY

    (newX, newY)

case class Expedition(time: Int, pos: (Int, Int)):
  val (x, y) = pos

  lazy val moves: Set[(Int, Int)] = Set(
    (x, y),
    (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ).filter { case coord @ (x, y) =>
    coord == Blizzard.startPos || coord == Blizzard.endPos || x > -1 && x < Blizzard.maxX && y > -1 && y < Blizzard.maxY
  }

  def possibleMoves(
      filledPositions: Map[Int, Set[(Int, Int)]]
  ): Set[Expedition] =
    moves
      .diff(filledPositions(time + 1))
      .map(Expedition(time + 1, _))

object Blizzard extends Base:
  val trueInput: String = input

  val maxX: Int = trueInput.toLines.map(_.length).max - 2
  val maxY: Int = trueInput.toLines.length - 2

  val startPos: (Int, Int) =
    val x = trueInput.toLines.head.zipWithIndex.find(_._1 == '.').get._2
    (x - 1, -1)

  val endPos: (Int, Int) =
    val x = trueInput.toLines.last.zipWithIndex.find(_._1 == '.').get._2
    (x - 1, maxY)

  private val blizzards: Set[Blizzard] =
    trueInput.toLines.zipWithIndex.flatMap { (row, y) =>
      row.zipWithIndex.flatMap { (char, x) =>
        char match
          case '>' => Some(Blizzard(x - 1, y - 1, (1, 0)))
          case '<' => Some(Blizzard(x - 1, y - 1, (-1, 0)))
          case '^' => Some(Blizzard(x - 1, y - 1, (0, -1)))
          case 'v' => Some(Blizzard(x - 1, y - 1, (0, 1)))
          case _   => None
      }.toSet
    }.toSet

  private val blizzardsPositionsAtTime: Map[(Blizzard, Int), (Int, Int)] =
    blizzards.flatMap { blizzard =>
      (1 to 1000).map { time =>
        (blizzard, time) -> blizzard.positionAtTime(time)
      }
    }.toMap

  private val filledPositionsEveryTime: Map[Int, Set[(Int, Int)]] =
    blizzardsPositionsAtTime
      .groupBy(_._1._2)
      .map { case (time, set) =>
        time -> set.values.toSet
      }
      .withDefaultValue(Set())

  private def stepsBetween(start: (Int, Int), time: Int, end: (Int, Int)): Int =
    @tailrec
    def loop(expeditions: Set[Expedition]): Int =
      if expeditions.exists(_.pos == end) then
        expeditions.find(_.pos == end).get.time
      else
        val newExpeditions = expeditions.flatMap { expedition =>
          expedition.possibleMoves(filledPositionsEveryTime)
        }
        loop(newExpeditions)

    loop(Set(Expedition(time, start)))

  def printMap(time: Int): Unit =
    val blizzardsInTime =
      blizzards
        .map(b => (b, b.positionAtTime(time)))
        .groupBy(_._2)
        .map { case (coord, set) =>
          coord -> set.map(_._1)
        }
        .withDefaultValue(Set())

    (-1 to maxY).foreach { y =>
      (-1 to maxX).foreach { x =>
        if (x == -1 || x == maxX || y == -1 || y == maxY) &&
          (x, y) != startPos && (x, y) != endPos
        then print('#')
        else
          blizzardsInTime((x, y)) match
            case set if set.size == 1 =>
              print(set.head.dir match
                case (1, 0)  => '>'
                case (-1, 0) => '<'
                case (0, -1) => '^'
                case (0, 1)  => 'v'
              )
            case set if set.size > 1 => print(set.size)
            case _                   => print('.')
      }
      println()
    }

  private lazy val timeToGoal: Int = stepsBetween(startPos, 0, endPos)
  private lazy val timeBackToStart: Int =
    stepsBetween(endPos, timeToGoal, startPos)
  private lazy val timeBackToGoal: Int =
    stepsBetween(startPos, timeBackToStart, endPos)

  override def part1: Any = timeToGoal
  override def part2: Any = timeBackToGoal

  @main def main(): Unit = run()
