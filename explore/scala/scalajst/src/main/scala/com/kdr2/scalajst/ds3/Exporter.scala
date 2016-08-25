package com.kdr2.scalajst.ds3

import scala.scalajs.js.annotation.JSExport

@JSExport
object Exporter {

  @JSExport
  def sphere(selector: String = "body", r: Double = 310.0): Unit = {
    val ball = new Sphere(r)
    ball.draw(selector)
  }

}
