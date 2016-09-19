
package mainapp


import resource.managed

import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.io.Source

import org.gephi.preview.plugin.renderers.EdgeRenderer


object qsBOsc {

  val mainPage = "http://www.geo.fu-berlin.de/en/met/ag/strat/produkte/qbo/"
  // we will not process this URL, which has older locations -Kanton Island and Gun, Maldives-,
  // because they have been updated for some years now.
  //
  //     http://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/qbo.dat
  //
  // Furthermore, the URL above, for Singapore, has more detailed measures, like the wind speed
  // for                    10, 12, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100 hPascal;
  // instead of only
  // for                    10, 15, 20, 30, 40, 50, 70 hPascal

  val measuresSingapore = "http://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/singapore.dat"

  // the two links below are not needed, because the data in singapore2015 and singapore2016 are
  // included in the singapore.dat URL above:
  //
  //           http://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/singapore2015.dat
  //           http://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/singapore2016.dat

  // type TimeSeriesWindSpeedByAtmosphPressure = Map[Int, Map[String, Int]]
  type TimeSeriesWindSpeedByAtmosphPressure = Map[String, Map[String, Int]]

  def main(cmdLineArgs: Array[String]) : Unit = {

    val timeSeries = parseSingaporeMeasures()

    println(scala.util.parsing.json.JSONObject(timeSeries))

  }

  // def parseSingaporeMeasures() : Map[Int, Map[String, Int]] = {

  def parseSingaporeMeasures(): TimeSeriesWindSpeedByAtmosphPressure = {

     println(s"Parsing: $measuresSingapore")

     // val timeSeries = MutableHashMap.empty[Int, MutableHashMap[String, Int]]
     val timeSeries = MutableHashMap.empty[String, MutableHashMap[String, Int]]

     var startNewBlock: Boolean = false
     var yearNewBlock: Int = -1

     for {inFile <- managed(Source.fromURL(measuresSingapore));
          inLine <- inFile.getLines()} {

       val fields = inLine.trim().split("\\s+")
       if (fields.length == 1) {
         if (fields(0).length == 0) {
           startNewBlock = true
         } else if (startNewBlock) {
           startNewBlock = false
           yearNewBlock = fields(0).toInt
         }
       } else if (fields.length > 1 && yearNewBlock > 1900 && fields(0) != "hPa") {
         startNewBlock = false
         // val atmosphPressure = fields(0).toInt
         val atmosphPressure = fields(0)
         if (! timeSeries.contains(atmosphPressure)) {
           timeSeries(atmosphPressure) = MutableHashMap.empty[String, Int]
         }

         for { month <- 1 until fields.length } {
           val timeSeriesKey = f"${yearNewBlock}%d${month}%02d"
           timeSeries(atmosphPressure).put(timeSeriesKey, fields(month).toInt)
         }
       }
     }

     timeSeries.map(kv => (kv._1,kv._2.toMap)).toMap
  }

}


// estimation of the altitude according to atmospheric air pressure:
//
//     http://www.mide.com/pages/air-pressure-at-altitude-calculator
//
// (the input time series do not give the altitude -which may be useful for
//  data visualization-, but give the raw atmospheric air pressure, which is
//  more exact.)

