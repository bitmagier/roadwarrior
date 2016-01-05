package org.purevalue.roadwarrior.algorithms

import org.purevalue.roadwarrior.{Location, Solution, CityMap, TravelingSalesmanAlg}

/**
  * @author Roman Krüger
  */
class ExposedVisitedEndNearestFirstAlg (cityMap: CityMap) extends TravelingSalesmanAlg (cityMap) {

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

  def statusNecessary (numVisited: Int) = {
    iterationsSinceLastStatus += 1
    numVisited < 11 && iterationsSinceLastStatus >= minIterationsBeforeStatusPrint && System.currentTimeMillis - lastStatusTimeMs > statusPrintAfterMs
  }

  def status (nodeVariants: Int, visited: List[Location], visitedDistance: Float) {
    print ("\r")
    if (solutionWay.nonEmpty)
      print ("solution way length: " + Math.round (solutionDistance) + " (" + solutionNodeVariants + " variants), ")
    print ("current: variants=" + nodeVariants + ", len=" + Math.round (visitedDistance) + " (" + visited.length + "/" + (numCities + 1) + ")")
    lastStatusTimeMs = System.currentTimeMillis ()
    iterationsSinceLastStatus = 0
  }


  def nearestRemaining (from: Location, remaining: Set[Location]): (Location, Float) = {
    conn (from).find (e => remaining.contains (e._1)).get
  }

  def nearestRemaining (from: Location, remaining: Set[Location], nodeVariants:Int): List[(Location, Float)] = {
    import scala.util.control.Breaks._
    val result = scala.collection.mutable.ListBuffer[(Location, Float)]() // $nodeVariants nearest locations available in $remaining
    breakable {
      for (next <- conn (from)) {
        if (remaining.contains (next._1)) {
          result.append(next)
          if (result.length == nodeVariants)
            break
        }
      }
    }
    result.toList
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
        status (nodeVariants, visited, visitedDistance)
      }
      return
    }

    if (statusNecessary (visited.length))
      status (nodeVariants, visited, visitedDistance)

    val (nearestFromHead, distHead) = nearestRemaining (visited.head, remaining)
    val (nearestFromLast, distLast) = nearestRemaining (visited.last, remaining)

    if (distHead >= distLast) {
      // append to head
      val candidates = nearestRemaining(visited.head, remaining, nodeVariants)
      for ((nextLocation, dist) <- candidates) {
        findShortest (nodeVariants, nextLocation +: visited, visitedDistance + dist, remaining - nextLocation)
      }
    } else {
      // append to last
      val candidates = nearestRemaining(visited.last, remaining, nodeVariants)
      for ((nextLocation, dist) <- candidates) {
        findShortest (nodeVariants, visited :+ nextLocation, visitedDistance + dist, remaining - nextLocation)
      }
    }
  }


  override def solve: Solution = {
    val startLocation = conn.keys.head
    for (variants <- 1 to conn.keySet.size - 1) {
      val startWay = List (conn (startLocation)(variants - 1)._1, startLocation)
      val startWayDistance = conn (startLocation)(variants - 1)._2
      val remaining = conn.keySet -- startWay
      findShortest (variants, startWay, startWayDistance, remaining)
    }

    Solution (cityMap, solutionWay.get)
  }
}
