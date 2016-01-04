package org.purevalue.roadwarrior

import scala.util.Random

/**
  * @author Roman Krüger
  */
class CityMapGenerator {
  val maxX = 1000
  val maxY = 1000

  def gen (numCities: Int): CityMap = {

    val locations: Set[Location] =
      List.range (1, numCities).map (x =>
        Location (x.toString, Random.nextInt(maxX), Random.nextInt(maxY))
      ).toSet

    CityMap (
      locations.map (a =>
        (a, (locations - a).map (x =>
          (x, distance (a, x))
        ).toMap)
      ).toMap
    )
  }

  def distance (a: Location, b: Location): Float = {
    val x = scala.math.abs (a.x - b.x)
    val y = scala.math.abs (a.y - b.y)
    scala.math.sqrt (x * x + y * y).toFloat
  }
}
