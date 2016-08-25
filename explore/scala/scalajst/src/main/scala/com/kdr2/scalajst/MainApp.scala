package com.kdr2.scalajst

import com.kdr2.scalajst.ds3.Sphere
import org.scalajs.dom.raw.HTMLDocument
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.d3

import scala.scalajs.js
import scala.scalajs.js.JSApp

class Rotate(var x: Int, var y: Int)

object MainApp extends JSApp {
  def main(): Unit = {
    println("Hello world!")
    // DOMGlobalScope.alert("hello")
    drawBars()
    val ball = new Sphere(310.0)
    ball.draw()
  }

  def drawBars(): Unit = {
    //The height of the chart.
    val graphHeight = 300

    //The width of each bar.
    val barWidth = 40

    //The distance between each bar.
    val barSeparation = 10

    //The maximum value of the data.
    val maxData = 50

    //The actual horizontal distance from drawing one bar rectangle to drawing the next.
    val horizontalBarDistance = barWidth + barSeparation

    //The value to multiply each bar's value by to get its height.
    val barHeightMultiplier = graphHeight / maxData;

    //Color for start
    val c = d3.rgb("DarkSlateBlue")

    val rectXFun = (d: Int, i: Int) => i * horizontalBarDistance
    val rectYFun = (d: Int) => graphHeight - d * barHeightMultiplier
    val rectHeightFun = (d: Int) => d * barHeightMultiplier
    val rectColorFun = (d: Int, i: Int) => c.brighter(i * 0.5).toString

    val svg = d3.select("body").append("svg").attr("width", "100%").attr("height", "450px")
    val sel = svg.selectAll("rect").data(js.Array(8, 22, 31, 36, 48, 17, 25))
    sel.enter()
      .append("rect")
      .attr("x", rectXFun)
      .attr("y", rectYFun)
      .attr("width", barWidth)
      .attr("height", rectHeightFun)
      .style("fill", rectColorFun)
  }
}

@js.native
object DOMGlobalScope extends js.GlobalScope {
  val document: HTMLDocument = js.native

  def alert(message: String): Unit = js.native
}