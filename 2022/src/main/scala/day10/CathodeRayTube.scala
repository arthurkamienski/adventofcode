package day10

import day10.CathodeRayTube.instructions
import utils.{Base, InputSource}

trait Instruction:
  val cycles: Int
  def execute(register: Int): Int

object Instruction:
  def fromString(s: String): Instruction = s match
    case "noop" => Noop()
    case add if add.take(4) == "addx" =>
      val x = add.split(" ").last.toInt
      Addx(x)

case class Addx(x: Int) extends Instruction:
  val cycles: Int = 2

  def execute(register: Int): Int = register + x

case class Noop() extends Instruction:
  val cycles: Int = 1

  def execute(register: Int): Int = register

case class CPU(
    cycles: Int = 1,
    register: Int = 1,
    history: Map[Int, Int] = Map.empty[Int, Int]
):
  def signalStrength(cycle: Int): Int = cycle * history(cycle)

  def drawPixel(cycle: Int, register: Int): Char =
    val pixelPos = (cycle - 1) % 40
    val isSpriteInPixel = (register - pixelPos).abs <= 1

    if isSpriteInPixel then '#' else '.'

  def drawImage: String =
    history.toSeq
      .sortBy(_._1)
      .init
      .map(drawPixel)
      .mkString
      .grouped(40)
      .mkString("\n")

object CPU:
  def execute(instructions: Seq[Instruction]): CPU =
    instructions.foldLeft(CPU()) { case (cpu, inst) =>
      val newCycles = cpu.cycles + inst.cycles
      val newRegister = inst.execute(cpu.register)
      val newHistory =
        cpu.history ++ (cpu.cycles to newCycles).map(_ -> cpu.register).toMap

      CPU(cycles = newCycles, register = newRegister, history = newHistory)
    }

object CathodeRayTube extends Base:
  override def inputSource: InputSource = InputSource("day10", isTest = false)

  def instructions: Seq[Instruction] = input.toLines.map(Instruction.fromString)

  def cpu: CPU = CPU.execute(instructions)

  override def part1: Any =
    (0 to 5).map(n => 20 + 40 * n).map(cpu.signalStrength).sum
  override def part2: Any = cpu.drawImage

  @main def main = run()
