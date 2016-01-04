package org.purevalue.roadwarrior.algorithms

import org.purevalue.roadwarrior.{Location, Solution, CityMap, TravelingSalesmanAlg}

/**
  * @author Roman Krüger
  */
class NearestFirstAlg (cityMap: CityMap) extends TravelingSalesmanAlg (cityMap) {

  val numCities = cityMap.connections.keySet.size
  // sorted after distance
  val conn: Map[Location, List[(Location, Float)]] = cityMap.connections.keySet.map (
    x => (x, cityMap.connections (x).toList.sortBy (_._2))
  ).toMap

  var solutionWay: Option[List[Location]] = None
  var solutionDistance = Float.PositiveInfinity
  var solutionNodeVariants: Int = 0
  val statusPrintAfterMs = 2000
  var lastStatusTimeMs: Long = System.currentTimeMillis
  val minIterationsBeforeStatusPrint = 1000
  var iterationsSinceLastStatus: Int = 0

  def statusNecessary (numVisited:Int) = {
    iterationsSinceLastStatus += 1
    numVisited < 11 && iterationsSinceLastStatus >= minIterationsBeforeStatusPrint && System.currentTimeMillis - lastStatusTimeMs > statusPrintAfterMs
  }

  def status (nodeVariants: Int, visited: List[Location], visitedDistance: Float) {
    print ("\r")
    if (solutionWay.nonEmpty)
      print ("solution way length: " + Math.round (solutionDistance) + " ("+solutionNodeVariants + " variants), ")
    print ("current: variants=" + nodeVariants + ", len=" + Math.round (visitedDistance) + " (" + visited.length + "/" + (numCities + 1) + ")")
    lastStatusTimeMs = System.currentTimeMillis ()
    iterationsSinceLastStatus = 0
  }


  def findShortest (nodeVariants: Int, visited: List[Location], visitedDistance: Float, remaining: Set[Location]): Unit = {
    if (visitedDistance >= solutionDistance)
      return

    if (remaining.isEmpty) {
      val fullDistance: Float = visitedDistance + cityMap.connections (visited.head)(visited.last)
      if (fullDistance < solutionDistance) {
        solutionWay = Some (visited.last +: visited)
        solutionDistance = fullDistance
        solutionNodeVariants = nodeVariants
        status(nodeVariants, visited, visitedDistance)
      }
      return
    }

    if (statusNecessary (visited.length))
      status (nodeVariants, visited, visitedDistance)


    var nextStationTries = List [(Location, Float)]() // $nodeVariants nearest locations available in $remaining

    import scala.util.control.Breaks._
    breakable {
      for (next <- conn (visited.head)) {
        if (remaining.contains (next._1)) {
          nextStationTries = next +: nextStationTries
          if (nextStationTries.length == nodeVariants)
            break
        }
      }
    }

    for ((nextLocation, dist) <- nextStationTries.reverse) {
      findShortest (nodeVariants, nextLocation +: visited, visitedDistance + dist, remaining - nextLocation)
    }
  }


  override def solve: Solution = {
    val startLocation = conn.keys.head
    for (variants <- 1 to conn.keySet.size - 1) {
      val startWay = conn (startLocation)(variants-1)._1 :: startLocation :: Nil
      val startWayDistance = conn (startLocation)(variants-1)._2
      val remaining = conn.keySet -- startWay
      findShortest (variants, startWay, startWayDistance, remaining)
    }

    Solution (cityMap, solutionWay.get)
  }
}
