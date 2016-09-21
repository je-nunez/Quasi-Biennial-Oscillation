
package mainapp

import java.io.{File, FileOutputStream}

import resource.managed

// scalastyle:off underscore.import
import scala.collection.JavaConverters._
// scalastyle:on underscore.import
import scala.collection.mutable.{ArrayBuffer, HashMap => MutableHashMap}
import scala.io.Source

// a Graphing Library for Java to visualize the Quasi-Bienal-Oscillation time series (an
// alternative for visualization could be JFreeChart)

// scalastyle:off illegal.imports
import java.awt.Color
// scalastyle:on illegal.imports
import de.erichseifert.gral.data.DataTable
import de.erichseifert.gral.graphics.Label
import de.erichseifert.gral.io.data.{DataWriter, DataWriterFactory}
import de.erichseifert.gral.io.plots.DrawableWriterFactory
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.plots.axes.LinearRenderer2D
import de.erichseifert.gral.plots.points.DefaultPointRenderer2D
import de.erichseifert.gral.util.GraphicsUtils

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

    val atmosphPressureToIndex = tsWS.keys.toList.sorted.zipWithIndex.toMap
    val numAtmosphPressure = atmosphPressureToIndex.keys.size
    val dataTable = new DataTable(numAtmosphPressure + 1, classOf[java.lang.Double])
    dataTable.setName("Quasi-Bienal-Oscillation by date and atmospheric pressure")

    timeYMs foreach {
      case (timeYM) => {
        val dataRow = ArrayBuffer.fill[java.lang.Double](numAtmosphPressure + 1)(0.0)

        print(timeYM)
        sortedTsWs.foreach {
          case (pressure, ts) => {
            val speed = if (ts.contains(timeYM)) ts(timeYM) else 0
            print(f"${pressure}%4s: ${speed}%4d")

            val pressureColIndex = atmosphPressureToIndex(pressure) + 1   // column is shifted by 1
            dataRow.update(pressureColIndex, 1.0 * speed)
          }
        }
        println
        dataRow.update(0, timeYM.toDouble)     // the first entry in row is X-value (the time)
        dataTable.add(dataRow.asJava)
      }
    }

    // TODO: fix scales on horizontal X axis (with the time), and change each atmospheric pressure
    //       to a different color.
    // plotTimeSeriesByYear(dataTable)
  }

  def plotTimeSeriesByYear(dataTable: DataTable): Unit = {
    // for debugging purposes only:
    // val outputFile = new FileOutputStream(new File("/tmp/text.csv"))
    // val dataWriter = DataWriterFactory.getInstance().get("text/csv")
    // dataWriter.write(dataTable, outputFile)

    val plot = new XYPlot(dataTable)
    plot.getTitle().setText(dataTable.getName)
    plot.setBackground(Color.WHITE)
    plot.getPlotArea().setBackground(Color.WHITE)

    val axisRendererX = plot.getAxisRenderer(XYPlot.AXIS_X)
    val axisRendererY = plot.getAxisRenderer(XYPlot.AXIS_Y)
    axisRendererX.setLabel(new Label("Time"))

    val axisLabelY = new Label("Wind Speed (0.1m/s)")
    axisLabelY.setRotation(90)
    axisRendererY.setLabel(axisLabelY)

    val defaultPointRenderer = new DefaultPointRenderer2D()
    val color = new Color(255, 0, 0)
    defaultPointRenderer.setColor(GraphicsUtils.deriveDarker(color))
    defaultPointRenderer.setErrorVisible(false)
    plot.setPointRenderers(dataTable, defaultPointRenderer)

    val (saveImgWidth, saveImgHeight) = (1200, 1600)

    plot.setBounds(0, 0, saveImgHeight, saveImgWidth)

    val imgWriter = DrawableWriterFactory.getInstance().get("image/png")
    imgWriter.write(plot,
                    new FileOutputStream(new File("/tmp/quasiBienalOscillation.png")),
                    saveImgHeight, saveImgWidth)

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

