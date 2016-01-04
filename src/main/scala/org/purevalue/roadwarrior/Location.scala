package org.purevalue.roadwarrior

import java.util.NoSuchElementException

/**
  * @author Roman Krüger
  */
case class Location(name:String, x:Int, y:Int)

case class CityMap(connections:Map[Location, Map[Location, Float]])
case class Solution(cityMap:CityMap, way:List[Location]) {
  val distance = way.sliding(2).foldLeft(0.0)((sum,elem) => sum + cityMap.connections(elem.head)(elem(1)))

  def checkSolution () = {
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

abstract class TravelingSalesmanAlg(cityMap:CityMap) {
  def solve: Solution
}