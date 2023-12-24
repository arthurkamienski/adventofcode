package day25

import utils.Base

import scala.annotation.tailrec

object HotAir extends Base:

  private def SNAFUtoInt(s: String): Long =
    def decodeSymbol(c: Char): Int = c match
      case '-' => -1
      case '=' => -2
      case _   => c.asDigit

    s.zipWithIndex.map { case (c, i) =>
      val v = decodeSymbol(c)
      val pow = s.length - 1 - i
      (v * math.pow(5, pow)).toLong
    }.sum

  private val totalFuel: Long = input.toLines.map(SNAFUtoInt).sum

  private def intToSNAFU(i: Long): String =
    @tailrec
    def toBase5(
        value: Long,
        currPower: Int,
        vals: Map[Int, Int]
    ): Map[Int, Int] =
      if value == 0 then vals
      else
        toBase5(
          value / 5,
          currPower + 1,
          vals.updated(currPower, (value % 5).toInt)
        )

    val base5 = toBase5(i, 0, Map.empty[Int, Int])

    base5.toSeq
      .sortBy(_._1)
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0)) {
        case (valsMap, (power, value)) =>
          val currValue = valsMap(power) + value
          val nextValue = valsMap(power + 1)

          if currValue >= 3 then
            val diff = currValue - 5

            valsMap.updated(power, diff).updated(power + 1, nextValue + 1)
          else valsMap.updated(power, currValue)
      }
      .toSeq
      .sortBy(-_._1)
      .map { case (_, v) =>
        v match
          case -1 => '-'
          case -2 => '='
          case _  => v.toString.head
      }
      .mkString("")

  override def part1: Any = intToSNAFU(totalFuel)

  override def part2: Any = ""

  @main def main(): Unit = run()
