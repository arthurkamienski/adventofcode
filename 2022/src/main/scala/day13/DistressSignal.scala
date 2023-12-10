package day13

import utils.{Base, InputSource}

import scala.annotation.{tailrec, targetName}

enum Order(val value: Int):
  case Ordered extends Order(1)
  case Unordered extends Order(-1)
  @targetName("questionMark")
  case ? extends Order(0)

import Order._

given Conversion[Int, Order] with
  def apply(i: Int): Order = i match
    case 1  => Ordered
    case -1 => Unordered
    case 0  => ?

given Conversion[Order, Int] with
  def apply(order: Order): Int = order.value

object Packet:
  def apply(i: Int): Packet = Packet(Left(i))
  def apply(packets: Seq[Packet]): Packet = Packet(Right(packets))

  def parse(s: String): Packet = parseNext(s)._1.head

  private def parseNext(s: String): (Seq[Packet], String) = s match
    case ""                 => (Seq(), "")
    case s if s.head == ']' => (Seq(), s.tail)
    case s if s.head == ',' => parseNext(s.tail)
    case s if s.head == '[' =>
      val (packetList, remaining) = parseNext(s.tail)
      val (packets, rest) = parseNext(remaining)

      (Packet(packetList) +: packets, rest)
    case _ =>
      val (n, remaining) = s.span(c => c != ',' && c != ']')
      val (packets, rest) = parseNext(remaining)

      (Packet(n.toInt) +: packets, rest)

case class Packet(content: Either[Int, Seq[Packet]]):
  def toSeqPacket: Packet = Packet(Right(Seq(Packet(content))))

object DistressSignal extends Base:
  override def inputSource: InputSource = InputSource("day13")

  private val packets: Seq[Packet] = input.toLines
    .filter(_ != "")
    .map(Packet.parse)

  private def packetPairs: Seq[(Packet, Packet)] = packets
    .grouped(2)
    .map(packets => (packets.head, packets(1)))
    .toSeq

  @tailrec
  private def compareLists(left: Seq[Packet], right: Seq[Packet]): Order =
    (left.headOption, right.headOption) match
      case (None, Some(_)) => Ordered
      case (Some(_), None) => Unordered
      case (None, None)    => ?
      case (Some(p1), Some(p2)) =>
        val res = compare(p1, p2)

        if res == ? then compareLists(left.tail, right.tail)
        else res

  @tailrec
  private def compare(left: Packet, right: Packet): Order =
    (left.content, right.content) match
      case (Left(a), Left(b)) =>
        if a > b then Unordered else if a < b then Ordered else ?
      case (Left(_), Right(_))  => compare(left.toSeqPacket, right)
      case (Right(_), Left(_))  => compare(left, right.toSeqPacket)
      case (Right(a), Right(b)) => compareLists(a, b)

  override def part1: Any =
    packetPairs
      .map(compare.tupled)
      .zipWithIndex
      .filter { case (order, _) => order == Ordered }
      .map { case (_, index) => index + 1 }
      .sum

  private val dividerPackets: Set[Packet] = Set("[[2]]", "[[6]]").map(Packet.parse)

  override def part2: Any =
    (packets ++ dividerPackets)
      .sorted((a, b) => compare(a, b).value)
      .reverse
      .zipWithIndex
      .filter { case (packet, _) => dividerPackets.contains(packet) }
      .map { case (_, index) => index + 1 }
      .product

  @main def main(): Unit = run()
