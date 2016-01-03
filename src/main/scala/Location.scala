/**
  * @author Roman Krüger
  */
case class Location(name:String, x:Int, y:Int)

case class CityMap(connections:Map[Location, Map[Location, Float]])
case class Solution(distance:Float, way:List[Location])

trait TravelingSalesmanAlg {
  def solve(cityMap:CityMap): Solution
}