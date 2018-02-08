package Weather

class ThermAdapter() extends IThermometer {

  def getMeanTemperature(cities: List[String]): Double = {
    val celsius = new CelsiusTherm;

    var avg = 0.0
    var total = 0.0
    for (city <- cities) { total += celsius.computeTemp(city) }

    avg = total / cities.length

    //convert to Farenheit

    avg = (avg * 1.8) + 32
    avg
  }
}

object WeatherStation extends App {
  val thermometer: IThermometer = new ThermAdapter

  val avgTemp =
    thermometer.getMeanTemperature(List("LA", "SF", "SLC", "Rio"))

  println("avg temp = " + avgTemp)
}