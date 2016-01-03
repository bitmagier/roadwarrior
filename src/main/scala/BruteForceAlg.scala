/**
  * @author Roman Krüger
  */
class BruteForceAlg extends TravelingSalesmanAlg {


  override def solve (cityMap: CityMap): Solution = {

    def buildWay (distanceVisited: Float, visited: List[Location], remaining: Set[Location]): Option[Solution] = {

      status(distanceVisited, visited)

      if (remaining.isEmpty) Some (Solution (distanceVisited, visited))
      else {
        var localSolution: Option[Solution] = None
        for (l <- remaining) {
          val hopDistance: Float = if (visited.isEmpty) 0 else  cityMap.connections (visited.head)(l)
          val currentDistance = distanceVisited + hopDistance
          val s = buildWay (currentDistance, l +: visited, remaining - l)
          if (s.nonEmpty) {
            if (localSolution.isEmpty || localSolution.get.distance > s.get.distance) localSolution = s
          }
        }
        localSolution
      }
    }

    def status (distanceVisited: Float, visited: List[Location]) = {
      if (visited.size == 1) println()
      if (visited.size == 2) {
        print("\r")
        print(visited(1) + " -> " + visited.head)
      }
    }

    val locations = cityMap.connections.keySet
    buildWay (0.0f, List.empty, locations).get
  }
}
