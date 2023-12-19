package day16

import utils.{Base, InputSource}

import scala.annotation.tailrec
import scala.collection.immutable

case class Valve(name: String, flowRate: Int)

object Volcanium extends Base:

  private val parsedInput: Set[(String, Int, Set[String])] =
    input.toLines.toSet.map { line =>
      val Pattern =
        """Valve (\w{2}) has flow rate=(\d+); tunnels? leads? to valves? ((?:\w{2}(?:, )?+)+)""".r

      val Pattern(name, flowRate, links) = line: @unchecked

      (name, flowRate.toInt, links.split(", ").toSet)
    }

  private val valves: Set[Valve] = parsedInput.map { case (name, flowRate, _) =>
    Valve(name, flowRate)
  }

  private val valveMap: Map[String, Valve] = valves.map(v => v.name -> v).toMap

  private val tunnels: Set[(Valve, Set[Valve])] = parsedInput.map {
    case (name, _, links) =>
      valveMap(name) -> links.map(valveMap)
  }

  private val nonZeroFlowValves: Set[Valve] = valves.filter(_.flowRate != 0)

  private val tunnelMap: TunnelMap = TunnelMap(tunnels, valves)

  @tailrec
  private def getAllPaths(
      states: Set[State],
      previousStates: Set[State] = Set.empty[State]
  ): Set[State] =
    val nextStates =
      states.flatMap(_.getNextStates(nonZeroFlowValves, tunnelMap))

    if nextStates == states then previousStates
    else getAllPaths(nextStates, previousStates = previousStates ++ states)

  private def getAllPathsFrom(
      initialValve: String,
      time: Int,
      openValves: Set[Valve] = Set.empty[Valve]
  ): Set[State] =
    getAllPaths(
      Set(
        State(
          valveMap(initialValve),
          time,
          0,
          Set(valveMap(initialValve)) ++ openValves
        )
      )
    )

  private def getBestPathFrom(
      initialValve: String,
      time: Int,
      openValves: Set[Valve] = Set.empty[Valve]
  ): State =
    getAllPathsFrom(initialValve, time, openValves).maxBy(_.pressureReleased)

  override def part1: Any = getBestPathFrom("AA", 30).pressureReleased

  override def part2: Any =
    val allPossibleStates = getAllPathsFrom("AA", 26)

    val maxPressurePerOpenValveGroup: Map[Set[Valve], Int] =
      allPossibleStates
        .groupBy(_.openValves)
        .map { case (valves, states) =>
          (valves - valveMap("AA")) -> states
            .maxBy(_.pressureReleased)
            .pressureReleased
        }

    maxPressurePerOpenValveGroup.flatMap { case (valves1, pressure1) =>
      maxPressurePerOpenValveGroup.flatMap { case (valves2, pressure2) =>
        if valves1.intersect(valves2).isEmpty then Some(pressure2 + pressure1)
        else None
      }
    }.max

  @main def main(): Unit = run()
