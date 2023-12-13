package day16

import utils.{Base, InputSource}

import scala.annotation.tailrec

case class TunnelMap(tunnels: Set[(Valve, Set[Valve])], valves: Set[Valve]):
  private type Edge = (Valve, Valve)
  private type TunnelGraph = Map[Edge, Int]

  private val initialGraph: TunnelGraph = tunnels.flatMap { case (from, tos) =>
    tos.map { to =>
      (from, to) -> 1
    }
  }.toMap

  private val graphWithoutUselessValves: TunnelGraph = valves
    .filter(v => v.flowRate == 0 && v.name != "AA")
    .foldLeft(initialGraph)(removeValveFromGraph)

  private def removeValveFromGraph(
      tunnels: TunnelGraph,
      valve: Valve
  ): TunnelGraph =
    val tunnelsFromValve = tunnels.view.filterKeys { case (from, _) =>
      from == valve
    }.toMap
    val tunnelsToValve = tunnels.view.filterKeys { case (_, to) =>
      to == valve
    }.toMap
    val otherTunnels = tunnels -- tunnelsFromValve.keys -- tunnelsToValve.keys

    val newTunnelsWithoutValve = tunnelsToValve.flatMap {
      case ((origin, _), distToValve) =>
        tunnelsFromValve.flatMap { case ((_, destination), distFromValve) =>
          val distOriginToDestination = distToValve + distFromValve

          if origin == destination then None
          else Some((origin, destination) -> distOriginToDestination)
        }
    }

    val updatedTunnels = otherTunnels.map { case (path, dist) =>
      newTunnelsWithoutValve.get(path) match
        case None              => path -> dist
        case Some(updatedDist) => path -> updatedDist.min(dist)
    }

    newTunnelsWithoutValve ++ updatedTunnels

  private def getDistance(from: Valve, to: Valve, graph: TunnelGraph): Int =
    if from == to then 0
    else
      graph.getOrElse(
        (from, to),
        100000 // could use Double.PositiveInfinity but no need for things to be double
      )

  val graph: TunnelGraph =
    val usefulValves = valves
      .filter(v => v.flowRate != 0 || v.name == "AA")

    usefulValves
      .foldLeft(graphWithoutUselessValves) { case (graph, via) =>
        usefulValves.foldLeft(graph) { case (graph, from) =>
          usefulValves.foldLeft(graph) { case (graph, to) =>
            val fromTo = getDistance(from, to, graph)
            val fromVia = getDistance(from, via, graph)
            val viaTo = getDistance(via, to, graph)

            graph + ((from, to) -> fromTo.min(fromVia + viaTo))
          }
        }
      }
      .view
      .mapValues(_ + 1) // add time to open the valve
      .toMap
