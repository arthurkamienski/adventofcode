package day02

import utils.{Base, InputSource}

enum Shape(val points: Int):
  case Rock extends Shape(1)
  case Paper extends Shape(2)
  case Scissors extends Shape(3)

  lazy val order: Seq[Shape] =
    Iterator.continually(Seq(Scissors, Rock, Paper)).flatten.take(5).toSeq

  def defeats: Shape = order(this.ordinal)
  def isDefeatedBy: Shape = order(this.ordinal + 2)

import Shape.*

enum Outcome(val points: Int):
  case Win extends Outcome(6)
  case Draw extends Outcome(3)
  case Loss extends Outcome(0)

object Outcome:
  def fromShapes(opponent: Shape, mine: Shape): Outcome = if opponent == mine
  then Draw
  else
    (opponent, mine) match
      case (Rock, Scissors)  => Loss
      case (Scissors, Paper) => Loss
      case (Paper, Rock)     => Loss
      case _                 => Win

import Outcome.*

case class Round(definition: String):
  private val (col1: Char, col2: Char) = (definition.head, definition.last)

  private def opponentShape: Shape = col1 match
    case 'A' => Rock
    case 'B' => Paper
    case 'C' => Scissors

  private def shapeFromInput: Shape = col2 match
    case 'X' => Rock
    case 'Y' => Paper
    case 'Z' => Scissors

  private def outcomeFromShapes: Outcome = Outcome.fromShapes(opponentShape, shapeFromInput)

  private def outcomeFromInput: Outcome = col2 match
    case 'X' => Loss
    case 'Y' => Draw
    case 'Z' => Win

  private def shapeFromOutcome: Shape = outcomeFromInput match
    case Draw => opponentShape
    case Win  => opponentShape.isDefeatedBy
    case Loss => opponentShape.defeats

  def part1Points: Int = points(shapeFromInput, outcomeFromShapes)
  def part2Points: Int = points(shapeFromOutcome, outcomeFromInput)

  private def points(shape: Shape, outcome: Outcome): Int =
    shape.points + outcome.points

object RockPaperScissors extends Base:

  private def rounds: Seq[Round] = input.toLines.map(Round.apply)

  override def part1: Int = rounds.map(_.part1Points).sum

  override def part2: Int = rounds.map(_.part2Points).sum

  @main def main(): Unit = run()
