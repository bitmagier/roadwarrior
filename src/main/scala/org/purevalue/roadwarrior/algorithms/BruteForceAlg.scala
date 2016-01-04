package org.purevalue.roadwarrior.algorithms

import org.purevalue.roadwarrior.{TravelingSalesmanAlg, Location, CityMap, Solution}

/**
  * @author Roman Krüger
  */
class BruteForceAlg (cityMap: CityMap) extends TravelingSalesmanAlg (cityMap) {

  override def solve: Solution = {

    var solutionWay: Option[List[Location]] = None // head = end
    var solutionDistance = Float.PositiveInfinity

    def buildWay (visitedDistance: Float, visited: List[Location], remaining: Set[Location]): Unit = {
      if (visitedDistance > solutionDistance)
        return

      if (remaining.isEmpty) {
        val fullDistance = visitedDistance + cityMap.connections (visited.head)(visited.last)
        if (fullDistance < solutionDistance) {
          solutionWay = Some (visited.last +: visited)
          solutionDistance = fullDistance
          status (visitedDistance, visited)
        }
        return
      }

      status (visitedDistance, visited)
      for (l <- remaining) {
        val hopDistance: Float = cityMap.connections (visited.last)(l)
        val currentDistance = visitedDistance + hopDistance
        buildWay (currentDistance, l +: visited, remaining - l)
      }
    }

    def status (distanceVisited: Float, visited: List[Location]) = {
      visited.size match {
        case 1 => println ()
        case 3 => print ("\rdistance="+Math.round(solutionDistance) + ", current:" + visited(2) + " -> " + visited (1) + " -> " + visited.head)
        case _ =>
      }
    }

    val locations = cityMap.connections.keySet
    val startLocation = locations.head
    buildWay (0.0f, List (startLocation), locations - startLocation)

    Solution(cityMap, solutionWay.get)
  }
}
