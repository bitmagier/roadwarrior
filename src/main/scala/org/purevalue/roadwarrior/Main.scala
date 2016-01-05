package org.purevalue.roadwarrior

import org.purevalue.roadwarrior.algorithms.{ExposedVisitedEndNearestFirstAlg, NearestFirstAlg, BruteForceAlg, Shortest2WayConnectionHeuristicAlg}

/**
  * @author Roman Krüger
  */
object Main {
  def main (args: Array[String]): Unit = {

    val cityMap = new CityMapGenerator ().gen (18)
    val algs: List[TravelingSalesmanAlg] = List (new ExposedVisitedEndNearestFirstAlg (cityMap), new NearestFirstAlg (cityMap), new Shortest2WayConnectionHeuristicAlg (cityMap)) //, new BruteForceAlg(cityMap))

    for (alg <- algs) {
      println ("Solving traveling salesman with " + alg.toString + " and " + (cityMap.connections.keySet.size + 1) + " locations")
      val timeStartMs = System.currentTimeMillis ()
      val solution = alg.solve
      val timeTakenMs = System.currentTimeMillis () - timeStartMs
      println
      println ("Solution found in " + Math.round (timeTakenMs / 1000) + " seconds.")
      println ("best distance=" + Math.round (solution.distance))
      println (solution)
    }
  }
}
