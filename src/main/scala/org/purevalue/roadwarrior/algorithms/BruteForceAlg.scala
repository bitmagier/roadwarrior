package org.purevalue.roadwarrior.algorithms

import java.time.{Duration, Instant}

import org.purevalue.roadwarrior.{BestSolution, CityMap, Location}

/**
  * @author bitmagier
  */
class BruteForceAlg(cityMap: CityMap, timeLimit: Duration) extends TravelingSalesmanAlg(cityMap, timeLimit) {
  private var startTime, timeOut: Instant = _
  private var solutionWay: Option[List[Location]] = None // head = end
  private var solutionDistance = Float.PositiveInfinity
  private var solutionDuration: Duration = _
  private val locations = cityMap.connections.keySet
  private val startLocation = locations.head

  private def timeIsUp: Boolean = Instant.now().isAfter(timeOut)

  def status(distanceVisited: Float, visited: List[Location]): Unit = {
    visited.size match {
      case 1 => println()
      case 3 => print("\rdistance=" + Math.round(solutionDistance) + ", current:" + visited(2) + " -> " + visited(1) + " -> " + visited.head)
      case _ =>
    }
  }

  def buildWay(visitedDistance: Float, visited: List[Location], remaining: Set[Location]): Unit = {
    if (timeIsUp) return

    if (visitedDistance > solutionDistance)
      return

    if (remaining.isEmpty) {
      val fullDistance = visitedDistance + cityMap.connections(visited.head)(visited.last)
      if (fullDistance < solutionDistance) {
        solutionWay = Some(visited.last +: visited)
        solutionDistance = fullDistance
        solutionDuration = Duration.between(startTime, Instant.now())
        status(visitedDistance, visited)
      }
      return
    }

    status(visitedDistance, visited)

    for (l <- remaining) {
      val hopDistance: Float = cityMap.connections(visited.last)(l)
      val currentDistance = visitedDistance + hopDistance
      buildWay(currentDistance, l +: visited, remaining - l)
    }
  }

  override def solve(): BestSolution = {
    startTime = Instant.now()
    timeOut = startTime.plus(timeLimit)

    buildWay(0.0f, List(startLocation), locations - startLocation)

    BestSolution(cityMap, solutionWay.get, solutionDuration, Duration.between(startTime, Instant.now()))
  }
}
