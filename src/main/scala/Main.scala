/**
  * @author Roman Krüger
  */
object Main {
  def main(args:Array[String]): Unit = {
    val cityMap = new CityMapGenerator().gen(12)
    val alg: TravelingSalesmanAlg = new BruteForceAlg
    println("solving TS with "+(cityMap.connections.keySet.size+1)+" locations")

    val timeStartMs = System.currentTimeMillis()
    val solution = alg.solve(cityMap)
    val timeTakenMs = System.currentTimeMillis() - timeStartMs
    println
    println("Best solution found in "+Math.round(timeTakenMs/1000)+" seconds: "+solution)
  }
}
