package com.kdr2.scalajst.ds3

import org.scalajs.dom._
import org.singlespaced.d3js.d3._
import org.singlespaced.d3js.geo.Graticule
import org.singlespaced.d3js.selection.Update
import org.singlespaced.d3js.{DragEvent, Selection, d3}

import scala.scalajs.js
import scala.scalajs.js.UndefOr


class Sphere(val width: Double) {

  import js.Tuple2.fromScalaTuple2

  type DatumFaction = Selection[Graticule]#DatumFunction[Primitive]

  val rotateData = new Sphere.Rotate(0, 90, -60)
  val projection = d3.geo.orthographic().translate((width / 2, width / 2)).clipAngle(90)
  val radius = projection((90.0, 0.0))._1 - projection((0.0, 0.0))._1;

  var container: Selection[EventTarget] = null
  var pathElement: Update[Graticule] = null


  def pathFunction: DatumFaction = {
    val path = d3.geo.path().projection(projection(_: js.Tuple2[Double, Double])).pointRadius(1.5)
    (x: Graticule, y: Int, z: UndefOr[Int]) => {
      val d = x.asInstanceOf[js.Dynamic]
      println(js.typeOf(x), x)
      console.log(x, y, z, d.selectDynamic("type"))
      path(x, y).asInstanceOf[Primitive]
    }
  }


  def fillPathData: Unit = {
    projection.rotate(js.Tuple3(-rotateData.x / 2, rotateData.y / 2, rotateData.z / 2))
    pathElement.attr("d", pathFunction)
  }

  def rotate(x: Int, y: Int, z: Int): Unit = {
    rotateData.x += x
    rotateData.y += y
    fillPathData
  }

  def draw(selector: String = "body"): Unit = {
    val graticule = d3.geo.graticule()
    graticule.precision(1.0)
    graticule.step((15.0, 25.0))

    container = d3.select(selector).append("svg").attr("width", "100%").attr("height", "450px")
    pathElement = container.append("path").attr("class", "graticule").datum(graticule)
    fillPathData

    val circle = container.append("circle").datum(rotateData).attr("r", radius)
      .attr("transform", s"translate(${width / 2}, ${width / 2})")
    val dragHandler = (_: Sphere.Rotate, _: Double) => {
      val e = d3.event.asInstanceOf[DragEvent]
      rotate(e.dx.toInt, e.dy.toInt, 0)
    }
    circle.call(d3.behavior.drag().on("drag", dragHandler))
  }

}

object Sphere {

  class Rotate(var x: Int, var y: Int, var z: Int)

}