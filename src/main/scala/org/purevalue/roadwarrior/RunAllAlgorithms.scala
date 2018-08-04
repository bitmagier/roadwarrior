package org.purevalue.roadwarrior

import java.time.Duration
import java.time.temporal.ChronoUnit

import org.purevalue.roadwarrior.algorithms._

/**
  * @author bitmagier
  */
object RunAllAlgorithms {

  private val TimeToSolvePerAlg = Duration.of(15, ChronoUnit.SECONDS)

  def main(args: Array[String]): Unit = {

    val cityMap = new CityMapGenerator ().gen (29)
    val algs: List[TravelingSalesmanAlg] = List (
      new ExposedVisitedEndNearestFirstAlg (cityMap, TimeToSolvePerAlg),
      new NearestFirstAlg (cityMap, TimeToSolvePerAlg),
      new Shortest2WayConnectionHeuristicAlg (cityMap, TimeToSolvePerAlg)) //, new BruteForceAlg(cityMap))

    for (alg <- algs) {
      println ("Solving traveling salesman with " + alg.toString + " and " + (cityMap.connections.keySet.size + 1) + " locations")
      val solution = alg.solve()
      println ("\n")
      println ("Best solution found after: " + solution.foundAfter)
      println ("Time consumed: " + solution.timeConsumed)
      println ("Shortest distance: " + Math.round (solution.distance))
      println (solution)
      println ("-------------------------------------------------")
    }
  }
}
