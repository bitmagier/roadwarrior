package org.purevalue.roadwarrior.algorithms

import java.time.{Duration, Instant}

import org.purevalue.roadwarrior.{BestSolution, CityMap, Location}


/**
  * @author bitmagier
  */
class ExposedVisitedEndNearestFirstAlg (cityMap: CityMap, timeLimit: Duration) extends TravelingSalesmanAlg (cityMap, timeLimit) {
  private var startTime, timeOut: Instant = _

  private val numCities = cityMap.connections.keySet.size
  // sorted after distance
  val conn: Map[Location, List[(Location, Float)]] = cityMap.connections.keySet.map (
    x => (x, cityMap.connections (x).toList.sortBy (_._2))
  ).toMap

  var solutionWay: Option[List[Location]] = None
  private var solutionDistance = Float.PositiveInfinity
  var solutionNodeVariants: Int = 0
  var solutionDuration: Duration = _
  val statusPrintAfterMs = 2000
  var lastStatusTimeMs: Long = System.currentTimeMillis
  val minIterationsBeforeStatusPrint = 1000
  var iterationsSinceLastStatus: Int = 0

  private def timeIsUp: Boolean = Instant.now().isAfter(timeOut)

  private def statusNecessary (numVisited: Int) = {
    iterationsSinceLastStatus += 1
    numVisited < 11 && iterationsSinceLastStatus >= minIterationsBeforeStatusPrint && System.currentTimeMillis - lastStatusTimeMs > statusPrintAfterMs
  }

  def status (nodeVariants: Int, visited: List[Location], visitedDistance: Float) {
    print ("\r")
    if (solutionWay.nonEmpty)
      print ("Solution so far: Way length=" + Math.round (solutionDistance) + " (" + solutionNodeVariants + " variants) ")
    print ("[Current: variations=" + nodeVariants + ", visitedDistance=" + Math.round (visitedDistance) + " (" + visited.length + "/" + (numCities + 1) + ")]")
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


  def findShortest (timeOut: Instant, nodeVariants: Int, visited: List[Location], visitedDistance: Float, remaining: Set[Location]): Unit = {
    def _findShortest(visited: List[Location], visitedDistance: Float, remaining: Set[Location]): Unit = {
      if (visitedDistance >= solutionDistance)
        return
      if (timeIsUp)
        return

      if (remaining.isEmpty) {
        val fullDistance: Float = visitedDistance + cityMap.connections(visited.head)(visited.last)
        if (fullDistance < solutionDistance) {
          solutionWay = Some(visited.last +: visited)
          solutionDistance = fullDistance
          solutionNodeVariants = nodeVariants
          solutionDuration = Duration.between(startTime, Instant.now())
          status(nodeVariants, visited, visitedDistance)
        }
        return
      }

      if (statusNecessary(visited.length))
        status(nodeVariants, visited, visitedDistance)

      val (nearestFromHead, distHead) = nearestRemaining(visited.head, remaining)
      val (nearestFromLast, distLast) = nearestRemaining(visited.last, remaining)

      if (distHead >= distLast) {
        // append to head
        val candidates = nearestRemaining(visited.head, remaining, nodeVariants)
        for ((nextLocation, dist) <- candidates) {
          _findShortest(nextLocation +: visited, visitedDistance + dist, remaining - nextLocation)
        }
      } else {
        // append to last
        val candidates = nearestRemaining(visited.last, remaining, nodeVariants)
        for ((nextLocation, dist) <- candidates) {
          _findShortest(visited :+ nextLocation, visitedDistance + dist, remaining - nextLocation)
        }
      }
    }

    _findShortest(visited, visitedDistance, remaining)
  }


  override def solve (): BestSolution = {
    val startLocation = conn.keys.head
    startTime = Instant.now
    timeOut = startTime.plus(timeLimit)

    for (variants <- 1 until conn.keySet.size) {
      val startWay = List (conn (startLocation)(variants - 1)._1, startLocation)
      val startWayDistance = conn (startLocation)(variants - 1)._2
      val remaining = conn.keySet -- startWay
      findShortest (startTime.plus(timeLimit), variants, startWay, startWayDistance, remaining)
    }

    BestSolution (cityMap, solutionWay.get, solutionDuration, Duration.between(startTime, Instant.now))
  }
}
