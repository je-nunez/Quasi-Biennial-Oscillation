
package mainapp

import java.lang.{Double => DoubleJava}
import java.io.{File, FileOutputStream}
import java.util.{ArrayList => ArrayListJava, HashMap => HashMapJava, List => ListJava,
                  Set => SetJava}

import resource.managed

// scalastyle:off underscore.import
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
// scalastyle:on underscore.import
import scala.collection.mutable.{ArrayBuffer, HashMap => MutableHashMap}
import scala.math
import scala.io.Source

// a Graphing Library for Java to visualize the Quasi-Bienal-Oscillation time series (an
// alternative for visualization could be JFreeChart)

// scalastyle:off illegal.imports
import java.awt.{Color, Font}
// scalastyle:on illegal.imports
import de.erichseifert.gral.data.{AbstractDataSource, DataSeries, DataTable}
import de.erichseifert.gral.graphics.{Insets2D, Label}
import de.erichseifert.gral.io.data.{DataWriter, DataWriterFactory}
import de.erichseifert.gral.io.plots.DrawableWriterFactory
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.plots.axes.{Axis, LinearRenderer2D, Tick}
import de.erichseifert.gral.plots.axes.Tick.TickType
import de.erichseifert.gral.plots.lines.DefaultLineRenderer2D
import de.erichseifert.gral.data.statistics.Statistics
import de.erichseifert.gral.util.GraphicsUtils

// The JTransforms Fast Fourier Transform
import org.jtransforms.fft.DoubleFFT_1D

import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.{Vector => SparkVector, DenseVector}
import org.apache.spark.rdd.RDD

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
    // the first two colums in the dataTable are the row index itself and the date YYYYMM
    val dataTable = new DataTable(numAtmosphPressure + 2, classOf[DoubleJava])
    dataTable.setName("Quasi-Bienal Equatorial Wind Speed Oscillation by date")

    timeYMs.zipWithIndex foreach {
      case (timeYM, index) => {
        val dataRow = ArrayBuffer.fill[DoubleJava](numAtmosphPressure + 2)(0.0)

        print(timeYM)
        sortedTsWs.foreach {
          case (pressure, ts) => {
            val speed = if (ts.contains(timeYM)) ts(timeYM) else 0
            print(f"${pressure}%4s: ${speed}%4d")

            val pressureColIndex = atmosphPressureToIndex(pressure) + 2   // column is shifted by 2
            dataRow.update(pressureColIndex, 1.0 * speed)
          }
        }
        println
        dataRow.update(0, index.toDouble)     // the first entry in row is the row index
        dataRow.update(1, timeYM.toDouble)     // the second entry in row is the time
        dataTable.add(dataRow.asJava)
      }
    }

    plotTimeSeriesByYear(dataTable, tsWS.keys.toArray.sorted)
  }

  def saveDataAsCsv(dataSource: AbstractDataSource, toCsvFile: String): Unit = {
    val csvMimeType = "text/csv"
    val outputFile = new FileOutputStream(new File(toCsvFile))
    val dataWriter = DataWriterFactory.getInstance().get(csvMimeType)
    dataWriter.write(dataSource, outputFile)
  }

  def savePlotAsPNG(plot: XYPlot, destFName: String, saveImgWidth: Int, saveImgHeight: Int):
    Unit = {
      plot.setBounds(0, 0, saveImgHeight, saveImgWidth)

      val imgWriter = DrawableWriterFactory.getInstance().get("image/png")
      val saveImgTo = new FileOutputStream(new File(destFName))
      imgWriter.write(plot, saveImgTo, saveImgHeight, saveImgWidth)
    }

  def calculateCustomAxisXDateTickLabels(dataTable: DataTable): Map[DoubleJava, String] = {

    val axisXTickLabels = MutableHashMap.empty[DoubleJava, String]
    val dateFirstSample = dataTable.get(1, 0).asInstanceOf[DoubleJava].toInt.toString
    axisXTickLabels += dataTable.get(0, 0).asInstanceOf[DoubleJava] -> dateFirstSample
    val monthFirstSample = dateFirstSample takeRight 2
    var countYearsPassedSinceLastTick: Int = 0
    for { row <- 1 until dataTable.getRowCount } {
      val dateSample = dataTable.get(1, row).asInstanceOf[DoubleJava].toInt.toString
      if ( dateSample.takeRight(2) == monthFirstSample ) {
        countYearsPassedSinceLastTick += 1
        if (countYearsPassedSinceLastTick == 1) {
          // draw the label of a tick every one year from the first year (for every one year,
          // the counter "countYearsPassedSinceLastTick" is not really necessary: it is for
          // a custom label not one per year, but only one label every a few years apart though)
          axisXTickLabels += dataTable.get(0, row).asInstanceOf[DoubleJava] -> dateSample
          countYearsPassedSinceLastTick = 0
        }
      }
    }

    axisXTickLabels.toMap
  }

  def createDefaultXYPlot(dataTable: DataTable, axisXTickLabels: Map[DoubleJava, String]):
    XYPlot = {
      val plot = new XYPlot()
      val (top, left, bottom, right) = (80.0, 80.0, 100.0, 20.0)
      plot.setInsets(new Insets2D.Double(top, left, bottom, right))
      plot.setBackground(Color.WHITE)
      plot.getPlotArea().setBackground(Color.WHITE)
      plot.getPlotArea().setBorderColor(Color.BLUE)

      val axisRendererX = new LinearRenderer2DNoMajorTicks()
      val axisRendererY = plot.getAxisRenderer(XYPlot.AXIS_Y)

      val axisLabelX = new Label("Time (YYYYMM)")
      axisLabelX.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 16))
      axisRendererX.setLabelDistance(axisRendererX.getLabelDistance + 3)
      axisRendererX.setLabel(axisLabelX)
      axisRendererX.setCustomTicks(new HashMapJava[DoubleJava,String](axisXTickLabels))
      axisRendererX.setTickLabelRotation(90)
      plot.setAxisRenderer(XYPlot.AXIS_X, axisRendererX)

      val axisLabelY = new Label("Avg Wind Speed (0.1m/s)")
      axisLabelY.setColor(Color.RED)
      axisLabelY.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 16))
      axisLabelY.setRotation(90)
      axisRendererY.setLabelDistance(axisRendererY.getLabelDistance + 1)
      axisRendererY.setLabel(axisLabelY)

      plot.getAxisRenderer(XYPlot.AXIS_X).setIntersection(-DoubleJava.MAX_VALUE)
      plot.getAxisRenderer(XYPlot.AXIS_Y).setIntersection(-DoubleJava.MAX_VALUE)

      plot
  }

  def findMinimumMaxWindSpeedsInData(dataTable: DataTable): (DoubleJava, DoubleJava) = {

    var (absoluteMin, absoluteMax) = (DoubleJava.POSITIVE_INFINITY, DoubleJava.NEGATIVE_INFINITY)

    // the first two colums in the dataTable are the row index itself and the date YYYYMM
    for { columnIndex <- 2 until dataTable.getColumnCount }{
      val column = dataTable.getColumn(columnIndex)

      val colMin = column.getStatistics(Statistics.MIN)
      val colMax = column.getStatistics(Statistics.MAX)

      if (colMin < absoluteMin) { absoluteMin = colMin }
      if (absoluteMax < colMax) { absoluteMax = colMax }
    }

    (absoluteMin, absoluteMax)
  }

  /*
  def hammingWindow(inputArray: Array[WindSpeed]): Array[DoubleJava] = {
    val length: Int = inputArray.length
    val twoPi = 2.0 * math.Pi

    inputArray.zipWithIndex.map(
      kv =>
         (( 0.53836 - ( 0.46164 * math.cos( (twoPi * kv._2) / (length - 1) ))) * kv._1): DoubleJava
    )

  }
   */

  def runFFTrealForwardFull1D(inputData: Array[WindSpeed]): (DoubleFFT_1D, Array[Double]) = {

    val realInputBuffer = ArrayBuffer.fill[Double](inputData.length * 2)(0.0)

    inputData.zipWithIndex.foreach {
      case (windSpeed, arrayIndex) => {
        realInputBuffer(arrayIndex) = (1.0 * windSpeed): Double
      }
    }

    val realInput = realInputBuffer.toArray

    val fft = new DoubleFFT_1D(inputData.length)

    fft.realForwardFull(realInput)

    (fft, realInput)
  }

  def runFFTrealForwardFull1D(fft: DoubleFFT_1D, inputData: Array[Double]): Array[Double] = {
    fft.complexInverse(inputData, true)

    inputData
  }

  def fftWindSpeedOscillationEstimation(windSpeed: DataSeries): DataTable = {

    val numbRows = windSpeed.getRowCount
    val windSpeedBuffer = ArrayBuffer.fill[WindSpeed](numbRows)(0)

    val numInputColumns = windSpeed.getColumnCount

    // the last column in the DataSeries is the wind speed
    val actualWindSpeeds = windSpeed.getColumn(numInputColumns - 1)
    for { row <- 0 until numbRows } {
      windSpeedBuffer(row) = actualWindSpeeds.get(row).asInstanceOf[DoubleJava].toInt
    }

    val inputArray = windSpeedBuffer.toArray
    val (fft1D, resultArray) = runFFTrealForwardFull1D(inputArray)

    val inversedData = runFFTrealForwardFull1D(fft1D, resultArray)

    // assemble a new DataTable with the results of the windSpeed DataSeries and a new column
    // with the FFT estimation

    val newDataTable = new DataTable(numInputColumns + 1, classOf[DoubleJava])

    for { row <- 0 until numbRows } {

      val oldRow = windSpeed.getRow(row)
      val newDataRow = ArrayBuffer.fill[DoubleJava](numInputColumns + 1)(0.0)

      for { col <- 0 until numInputColumns } {
        newDataRow.update(col, oldRow.get(col).asInstanceOf[DoubleJava])
      }

      // complexInverse(...) for the Fast Fourier Transform returns in (index*2) the inversed
      // real values (and in index*2 + 1, the imaginary part)
      newDataRow.update(numInputColumns, inversedData(row * 2))

      newDataTable.add(newDataRow.asJava)
    }

    newDataTable
  }


  def plotAtmosphericPressureByYear(dataTable: DataTable, atmosphPressure: Int,
                                    colAtmosphPressureInSeries: Int,
                                    axisXTickLabels: Map[DoubleJava, String],
                                    absMinWindSpeed: DoubleJava, absMaxWindSpeed: DoubleJava,
                                    colorToPlot: Color)
    : Unit = {

      val colStdXValue = 0

      val actualColumnInDataTable = colAtmosphPressureInSeries + 2
      val dataSeriesAtmosphPressure = new DataSeries(atmosphPressure.toString, dataTable,
                                                     colStdXValue, actualColumnInDataTable)

      val newDataAtmosphPressureFFT = fftWindSpeedOscillationEstimation(dataSeriesAtmosphPressure)

      // for debugging purposes only: TODO: make it portable to Windows, ie., no /tmp/
      saveDataAsCsv(newDataAtmosphPressureFFT, s"/tmp/speedAtPressure_${atmosphPressure}.csv")

      val plot = createDefaultXYPlot(dataTable, axisXTickLabels)
      plot.add(0, newDataAtmosphPressureFFT, true)
      val lineRendered = new DefaultLineRenderer2D()
      // lineRendered.setColor(GraphicsUtils.deriveDarker(colorToPlot))
      lineRendered.setColor(colorToPlot)
      plot.setLineRenderers(newDataAtmosphPressureFFT, lineRendered)

      // standarize all Y axis in all generated images to the same numm range of wind speeds,
      // from "absMinWindSpeed" up to "absMaxWindSpeed" (plus 5% for borders)
      val emptySpaceYBorders = 0.05
      plot.getAxis(XYPlot.AXIS_Y).setRange(absMinWindSpeed * (1 + emptySpaceYBorders),
                                           absMaxWindSpeed * (1 + emptySpaceYBorders))

      val labelTitle = plot.getTitle()
      labelTitle.setText(dataTable.getName +
                         s", at atmospheric pressure ${atmosphPressure} hPascals")
      labelTitle.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 24))
      labelTitle.setColor(Color.BLACK)

      savePlotAsPNG(plot, s"/tmp/quasiBienalOscillation_${atmosphPressure}.png", 1200, 1600)
      // if you want to re-use the plot, and not to created anew in each iteration, just do:
      //     plot.remove(qnewDataAtmosphPressureFFT)
      // and return the plot to the caller, so the caller can re-use it for other tasks
  }

  def plotTimeSeriesByYear(dataTable: DataTable, columnsLegend: Array[AtmosphPressure]): Unit = {

    val axisXTickLabels = calculateCustomAxisXDateTickLabels(dataTable)

    val (absMinSpeed, absMaxSpeed) = findMinimumMaxWindSpeedsInData(dataTable)

    // Plots can be re-used, so they do not need to be created at every iteration per atmospheric
    // pressure:
    // val plot = createDefaultXYPlot(dataTable, axisXTickLabels)

    val minColor = new Color(0x000000ff)
    val maxColor = new Color(0x00ff0000)  // range of colors for plotting each serie
    val totalColorsNeeded = columnsLegend.length

    columnsLegend.zipWithIndex foreach {
      case (atmosphPressure, colAtmosphPressure) => {

           val colorToPlot = GraphicsUtils.blend(minColor, maxColor,
                                                 (1.0 * colAtmosphPressure)/totalColorsNeeded)

           plotAtmosphericPressureByYear(dataTable, atmosphPressure, colAtmosphPressure,
                                         axisXTickLabels, absMinSpeed, absMaxSpeed,
                                         colorToPlot)
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

  def convertDataSource2RDD(inpDataSrc: AbstractDataSource, sc: SparkContext): RDD[SparkVector] = {

    val numbRows = inpDataSrc.getRowCount
    val numbCols = inpDataSrc.getColumnCount
    val bufferArray = new ArrayBuffer[SparkVector](numbRows)

    for { rowIdx <- 0 until numbRows } {

      val inputRow = inpDataSrc.getRow(rowIdx)
      val rddRow = new ArrayBuffer[Double](numbCols)

      for { col <- 0 until numbCols } {
        rddRow.update(col, inputRow.get(col).asInstanceOf[DoubleJava])
      }

      bufferArray(rowIdx) = new DenseVector(rddRow.toArray)
    }

    val rdd = sc.parallelize(bufferArray).cache()

    rdd
  }

  class LinearRenderer2DNoMajorTicks extends LinearRenderer2D {

    protected override def createTicks(ticks: ListJava[Tick], axis: Axis,
                                       min: Double, max: Double,
                                       tickPositions: SetJava[DoubleJava],
                                       isAutoSpacing: Boolean): Unit = {

      val ticksMajorMinor = new ArrayListJava[Tick]()

      super.createTicks(ticksMajorMinor, axis, min, max, tickPositions, isAutoSpacing)

      // lower all the Major Ticks, because the major ticks have labels and
      // we only need to paint the custom labels, not the major labels. I.e,
      // convert all the Major Ticks to Minor Ticks

      val allTicksMinor =
        ticksMajorMinor.map(
                t => {
                       if ( t.`type` == TickType.MAJOR ) {
                         // convert the major tick to a minor tick (and no label string)
                         val replacementMinor = new Tick(TickType.MINOR, t.position, t.normal,
                                                         t.drawable, t.shape, "")
                         replacementMinor
                       } else {
                         t
                       }
                     }
        )

      // ticks.addAll(allTicksMinor)
    }

  }

}


// estimation of the altitude according to atmospheric air pressure:
//
//     http://www.mide.com/pages/air-pressure-at-altitude-calculator
//
// (the input time series do not give the altitude -which may be useful for
//  data visualization-, but give the raw atmospheric air pressure, which is
//  more exact.)

