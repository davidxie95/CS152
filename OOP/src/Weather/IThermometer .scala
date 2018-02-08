package Weather
import scala.collection.immutable.List

trait IThermometer {
   // = avg degrees Farenheit
   def getMeanTemperature(cities : List[String]): Double
}