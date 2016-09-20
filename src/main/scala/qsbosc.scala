
package mainapp


import resource.managed

import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.io.Source

import org.gephi.preview.plugin.renderers.EdgeRenderer


object qsBOsc {

  val mainPage = "http://www.geo.fu-berlin.de/en/met/ag/strat/produkte/qbo/"
  // We will not process this data URL:
  //
  //     http://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/qbo.dat
  //
  // which has older locations -Kanton Island and Gun, Maldives-, because they have not been
  // updated for some years now.
  //
  // Furthermore, the URL below, for Singapore, has more detailed measures, like the wind speed
  // for                    10, 12, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100 hPascal;
  // instead of only
  // for                    10, 15, 20, 30, 40, 50, 70 hPascal

  val measuresSingapore = "http://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/singapore.dat"

  // the two links below are not needed, because the data in singapore2015 and singapore2016 are
  // included in the singapore.dat URL above:
  //
  //           http://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/singapore2015.dat
  //           http://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/singapore2016.dat

  type AtmosphPressure = Int
  // type AtmosphPressure = String
  type TimeStamp = String
  type WindSpeed = Int

  type TimeSeriesWindSpeedByAtmosphPressure = Map[AtmosphPressure, Map[TimeStamp, WindSpeed]]

  def main(cmdLineArgs: Array[String]) : Unit = {

    val timeSeries = parseSingaporeMeasures()

    processTimeSeries(timeSeries)

  }

  def processTimeSeries(tsWS: TimeSeriesWindSpeedByAtmosphPressure): Unit = {

    reportTimeSeriesByPressure(tsWS)

    reportTimeSeriesByYear(tsWS)
  }

  def reportTimeSeriesByPressure(tsWS: TimeSeriesWindSpeedByAtmosphPressure): Unit = {

    println("Reporting the Quasi-Bienal-Oscillation according atmospheric pressure first.")

    tsWS.toSeq.sortBy(_._1).foreach {
      case (pressure, ts) => {
        print(pressure)
        ts.toSeq.sortBy(_._1).foreach {
          case(dateYM, windSpeed) => print(s" ${dateYM}: $windSpeed,")
          case _ =>
        }
        println
      }
      case _ =>
    }
  }

  def reportTimeSeriesByYear(tsWS: TimeSeriesWindSpeedByAtmosphPressure): Unit = {

    println("Reporting the Quasi-Bienal-Oscillation according to date first.")

    val timeYMs = tsWS.values.map(_.keys.toList).flatten.toList.distinct.sorted
    val sortedTsWs = tsWS.toSeq.sortBy(_._1)

    timeYMs foreach {
      case (timeYM) => {
        print(timeYM)
        sortedTsWs.foreach {
          case (pressure, ts) => {
            val speed = if (ts.contains(timeYM)) ts(timeYM) else 0
            print(f"${pressure}%4s: ${speed}%4d")
          }
        }
        println
      }
    }
  }

  def parseSingaporeMeasures(): TimeSeriesWindSpeedByAtmosphPressure = {

     println(s"Parsing: $measuresSingapore")

     val timeSeries = MutableHashMap.empty[AtmosphPressure, MutableHashMap[TimeStamp, WindSpeed]]

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
         val atmosphPressure = fields(0).toInt
         // val atmosphPressure = fields(0)
         if (! timeSeries.contains(atmosphPressure)) {
           timeSeries(atmosphPressure) = MutableHashMap.empty[TimeStamp, WindSpeed]
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

