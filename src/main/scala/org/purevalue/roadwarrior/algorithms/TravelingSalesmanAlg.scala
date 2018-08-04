package org.purevalue.roadwarrior.algorithms

import java.time.Duration

import org.purevalue.roadwarrior.{BestSolution, CityMap}

abstract class TravelingSalesmanAlg(cityMap:CityMap, timeLimit: Duration) {
  def solve(): BestSolution
  override def toString: String = getClass.getSimpleName
}
