package day06

import utils.{Base, InputSource}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class CommsDevice(signal: String):
  private def indexedSignal: Seq[(Char, Int)] = signal.zipWithIndex

  private def findFirstNonRepeating(seqSize: Int): Int =
    @tailrec
    def find(buffer: Queue[Char], sequence: Seq[(Char, Int)]): Int =
      val (char, charPos) = sequence.head
      val newBuffer = buffer.enqueue(char)
      val (_, nextBuffer) = newBuffer.dequeue

      if newBuffer.toSet.size == seqSize then charPos
      else find(nextBuffer, sequence.tail)

    find(
      Queue.from(signal.take(seqSize - 1)),
      indexedSignal.drop(seqSize - 1)
    ) + 1

  def findStartOfPacket: Int = findFirstNonRepeating(4)
  def findStartOfMessage: Int = findFirstNonRepeating(14)

object Tuning extends Base:

  override def part1: Any = CommsDevice(input).findStartOfPacket

  override def part2: Any = CommsDevice(input).findStartOfMessage

  @main def main(): Unit = run()
