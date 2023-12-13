package day16

case class State(
    currentValve: Valve,
    timeLeft: Int,
    pressureReleased: Int,
    openValves: Set[Valve]
):
  def getNextStates(
      nonZeroFlowValves: Set[Valve],
      tunnelMap: TunnelMap
  ): Set[State] =
    lazy val nextStates = nonZeroFlowValves
      .diff(openValves)
      .flatMap { nextValve =>
        val timeToOpenNextValve = tunnelMap.graph((currentValve, nextValve))
        val timeAfterOpeningNextValve = timeLeft - timeToOpenNextValve

        if timeAfterOpeningNextValve <= 0 then None
        else
          val pressureReleasedAfterOpeningNextValve =
            nextValve.flowRate * timeAfterOpeningNextValve

          Some(
            State(
              currentValve = nextValve,
              timeLeft = timeAfterOpeningNextValve,
              pressureReleased =
                pressureReleased + pressureReleasedAfterOpeningNextValve,
              openValves = openValves + nextValve
            )
          )
      }

    if timeLeft <= 2 || nextStates.isEmpty then Set(this)
    else nextStates
