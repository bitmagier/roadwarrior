package org.purevalue.roadwarrior

import java.time.Duration


/**
  * @author bitmagier
  */
case class Location(name:String, x:Int, y:Int)

case class CityMap(connections:Map[Location, Map[Location, Float]])

abstract class ASolution(cityMap:CityMap, way:List[Location], solutionFoundAfter: Duration)
{
  val distance: Double = way.sliding(2).foldLeft(0.0)((sum,elem) => sum + cityMap.connections(elem.head)(elem(1)))

  private def checkSolution(): Unit = {
    var remaining = cityMap.connections.keySet
    if (way.head != way.last) {
      throw new Exception("Way is not ending where it began")
    }
    for (l <- way.take(way.length-1)) {
      if (remaining.contains(l)) {
        remaining = remaining - l
      } else {
        throw new Exception(l+" not contained in "+remaining)
      }
    }
    if (remaining.nonEmpty) {
      throw new Exception(remaining + " not visited")
    }
  }

  checkSolution()
}

case class Solution(cityMap:CityMap, way:List[Location], foundAfter: Duration) extends ASolution(cityMap, way, foundAfter)
case class BestSolution(cityMap:CityMap, way:List[Location], foundAfter: Duration, timeConsumed: Duration) extends ASolution(cityMap, way, foundAfter)