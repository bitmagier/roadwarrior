/**
  * @author Roman Krüger
  */
object Main {
  def main (args: Array[String]): Unit = {

    // val algs: List[TravelingSalesmanAlg] = List (new BruteForceAlg, new NeighboursFirstAlg)
    val algs: List[TravelingSalesmanAlg] = List (new NeighboursFirstAlg)

    val cityMap = new CityMapGenerator ().gen (20)
    for (alg <- algs) {
      println ("Solving traveling salesman with " + alg.toString + " and " + (cityMap.connections.keySet.size + 1) + " locations")
      val timeStartMs = System.currentTimeMillis ()
      val solution = alg.solve (cityMap)
      val timeTakenMs = System.currentTimeMillis () - timeStartMs
      println
      println ("Best solution found in " + Math.round (timeTakenMs / 1000) + " seconds: " + solution)
    }
  }
}
