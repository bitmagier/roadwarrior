package org.purevalue.roadwarrior.algorithms

import org.purevalue.roadwarrior.{Location, Solution, CityMap, TravelingSalesmanAlg}

/**
  * @author Roman Krüger
  */
class Shortest2WayConnectionHeuristicAlg (cityMap:CityMap) extends TravelingSalesmanAlg(cityMap) {

  override def solve: Solution = {

    // map values sorted after distance
    val conn: Map[Location, List[(Location, Float)]] = cityMap.connections.keySet.map (
      x => (x, cityMap.connections (x).toList.sortBy (_._2))
    ).toMap

    def firstWay (): Solution = {
      var fragments = List [List[Location]]()
      var locationsLeft = conn.keySet

      var f = List [Location]()
      do {
        f = shortestUnconnected2WayVisit (locationsLeft)
        if (f.nonEmpty) {
          fragments = f +: fragments
          locationsLeft = locationsLeft -- f
        }
      } while (f.nonEmpty)

      val unconnectedLocations = conn.keySet -- fragments.flatten
      fragments = fragments ++ unconnectedLocations.map (List (_))

      assembleFragments (fragments.toSet)
    }

    def shortestUnconnected2WayVisit (locLeft: Set[Location]): List[Location] = {
      val locLeftFiltered = locLeft.filter (x => locLeft.contains (conn (x).head._1) && locLeft.contains (conn (x)(1)._1))
      if (locLeftFiltered.isEmpty) List ()
      else {
        val l = locLeftFiltered.minBy (x => conn (x).head._2 + conn (x)(1)._2)
        List (conn (l).head._1, l, conn (l)(1)._1)
      }


    }

    // assemble fragments in any order of fragments and fragment direction
    def assembleFragments (fragments: Set[List[Location]]): Solution = {
      buildWay (0f, List (), fragments)
    }

    def buildWay (visitedLength: Float, visited: List[Location], fragments: Set[List[Location]]): Solution = {
      if (fragments.isEmpty) {
        Solution (cityMap, visited :+ visited.head)
      }
      else {
        var localSolution: Solution = null
        for (f <- fragments) {
          val s1 = buildWay (visitedLength + (if (visited.isEmpty) 0 else distance (visited.last :: f.head :: Nil)) + distance (f), visited ++ f, fragments - f)
          if (localSolution == null || localSolution.distance > s1.distance)
            localSolution = s1
          if (f.length > 1) {
            val fReversed = f.reverse
            val s2 = buildWay (visitedLength + (if (visited.isEmpty) 0 else distance (visited.last :: fReversed.head :: Nil)) + distance (fReversed), visited ++ fReversed, fragments - f)
            if (localSolution.distance > s2.distance) {
              localSolution = s2
            }
          }
        }
        localSolution
      }
    }

    def distance (way: List[Location]): Float = distance2 (way.head, way.tail)
    def distance2 (a: Location, rest: List[Location]): Float = {
      if (rest.isEmpty) 0
      else cityMap.connections (a)(rest.head) + distance2 (rest.head, rest.tail)
    }

    firstWay ()
  }

}
