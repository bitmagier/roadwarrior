package org.purevalue.roadwarrior.algorithms

import java.time.{Duration, Instant}

import org.purevalue.roadwarrior.{BestSolution, CityMap, Location, Solution}

/**
  * @author bitmagier
  */
class Shortest2WayConnectionHeuristicAlg(cityMap: CityMap, timeLimit: Duration) extends TravelingSalesmanAlg(cityMap, timeLimit) {
  private var startTime, timeOut: Instant = _

  // map values sorted after distance
  val conn: Map[Location, List[(Location, Float)]] = cityMap.connections.keySet.map(
    x => (x, cityMap.connections(x).toList.sortBy(_._2))
  ).toMap

  private def timeIsUp: Boolean = Instant.now().isAfter(timeOut)

  def distance2(a: Location, rest: List[Location]): Float = {
    if (rest.isEmpty) 0
    else cityMap.connections(a)(rest.head) + distance2(rest.head, rest.tail)
  }

  def distance(way: List[Location]): Float = distance2(way.head, way.tail)

  def shortestUnconnected2WayVisit(locLeft: Set[Location]): List[Location] = {
    val locLeftFiltered = locLeft.filter(x => locLeft.contains(conn(x).head._1) && locLeft.contains(conn(x)(1)._1))
    if (locLeftFiltered.isEmpty) List()
    else {
      val l = locLeftFiltered.minBy(x => conn(x).head._2 + conn(x)(1)._2)
      List(conn(l).head._1, l, conn(l)(1)._1)
    }
  }

  def buildWay(visitedLength: Float, visited: List[Location], fragments: Set[List[Location]]): Option[Solution] = {
    if (fragments.isEmpty) {
      val solutionDuration = Duration.between(startTime, Instant.now())
      Some(Solution(cityMap, visited :+ visited.head, solutionDuration))
    }
    else if (timeIsUp) None
    else {
      var localSolution: Option[Solution] = None
      for (f <- fragments) {
        val s1 = buildWay(visitedLength + (if (visited.isEmpty) 0 else distance(visited.last :: f.head :: Nil)) + distance(f), visited ++ f, fragments - f)
        if (localSolution.isEmpty || (s1.isDefined && localSolution.get.distance > s1.get.distance))
          localSolution = s1
        if (f.length > 1) {
          val fReversed = f.reverse
          val s2 = buildWay(visitedLength + (if (visited.isEmpty) 0 else distance(visited.last :: fReversed.head :: Nil)) + distance(fReversed), visited ++ fReversed, fragments - f)
          if (s2.isDefined && (localSolution.isEmpty || localSolution.get.distance > s2.get.distance)) {
            localSolution = s2
          }
        }
      }
      localSolution
    }
  }

  // assemble fragments in any order of fragments and fragment direction
  def assembleFragments(fragments: Set[List[Location]]): Option[Solution] = {
    buildWay(0f, List(), fragments)
  }

  def firstWay(): Solution = {
    var fragments = List[List[Location]]()
    var locationsLeft = conn.keySet

    var f = List[Location]()
    do {
      f = shortestUnconnected2WayVisit(locationsLeft)
      if (f.nonEmpty) {
        fragments = f +: fragments
        locationsLeft = locationsLeft -- f
      }
    } while (f.nonEmpty && !timeIsUp)

    val unconnectedLocations = conn.keySet -- fragments.flatten
    fragments = fragments ++ unconnectedLocations.map(List(_))

    assembleFragments(fragments.toSet).get
  }


  def timeConsumed: Duration = Duration.between(startTime, Instant.now())

  override def solve(): BestSolution = {
    startTime = Instant.now
    timeOut = startTime.plus(timeLimit)

    val s: Solution = firstWay()
    BestSolution(s.cityMap, s.way, s.foundAfter, timeConsumed)
  }
}
