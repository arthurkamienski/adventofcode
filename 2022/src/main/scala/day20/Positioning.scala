package day20

import utils.Base

import scala.annotation.tailrec

case class Number(value: BigInt, pos: Int)

object Positioning extends Base:
  private val numbers: Seq[Number] =
    input.toLines
      .map(n => BigInt(n.toInt))
      .zipWithIndex
      .map(Number.apply.tupled)
  private val numbersDecryptionKey: Seq[Number] =
    numbers.map(n => n.copy(value = n.value * 811589153))

  private val numberPositions: Map[Number, Int] = numbers.zipWithIndex.toMap
  private val numberPositionsDecryptionKey: Map[Number, Int] =
    numbersDecryptionKey.zipWithIndex.toMap

  private def getPosition(i: BigInt): Int = (i % numbers.size).toInt

  private def positionAdjustment(n: BigInt): Int =
    if n >= numbers.size then (n / numbers.size).toInt
    else if n < 0 then (n / numbers.size).toInt
    else 0

  @tailrec
  private def getNextPosition(n: BigInt): Int =
    val pos = getPosition(n) + positionAdjustment(n)

    if pos < numbers.size && pos >= 0 then pos
    else if pos.abs < numbers.size && pos < 0 then pos + numbers.size - 1
    else getNextPosition(pos)

  private def mixNumbers(
      times: Int,
      numbers: Seq[Number],
      numberPositions: Map[Number, Int]
  ): Map[Number, Int] =
    (1 to times).foldLeft(numberPositions) { case (positions, _) =>
      numbers.foldLeft(positions) { case (positions, num) =>
        val positionNumbers = positions.map(_.swap)

        val currPosition = positions(num)
        val nextPosition = getNextPosition(currPosition + num.value)

        val indexesToAdjust =
          if nextPosition < currPosition then
            (nextPosition until currPosition)
              .map(n => positionNumbers(n) -> (n + 1))
              .toMap
          else
            (currPosition + 1 to nextPosition)
              .map(n => positionNumbers(n) -> (n - 1))
              .toMap

        positions.updated(num, nextPosition) ++ indexesToAdjust
      }
    }

  private lazy val numbersAfterMix: Map[Number, Int] =
    mixNumbers(1, numbers, numberPositions)
  private lazy val numbersDecryptionKeyAfterMix: Map[Number, Int] = mixNumbers(
    10,
    numbersDecryptionKey,
    numberPositionsDecryptionKey
  )

  private def coordinates(
      numbers: Seq[Number],
      numbersAfterMix: Map[Number, Int]
  ): BigInt =
    val positionNumbers = numbersAfterMix.map(_.swap)
    val zeroPosition = numbersAfterMix(numbers.find(_.value == 0).get)

    positionNumbers(getPosition(zeroPosition + 1000)).value +
      positionNumbers(getPosition(zeroPosition + 2000)).value +
      positionNumbers(getPosition(zeroPosition + 3000)).value

  override def part1: Any = coordinates(numbers, numbersAfterMix)
  override def part2: Any =
    coordinates(numbersDecryptionKey, numbersDecryptionKeyAfterMix)

  @main def main(): Unit = run()
