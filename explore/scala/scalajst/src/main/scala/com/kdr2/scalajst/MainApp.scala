package com.kdr2.scalajst

import org.scalajs.dom.raw.HTMLDocument

import scala.scalajs.js
import scala.scalajs.js.JSApp

object MainApp extends JSApp {
  def main(): Unit = {
    println("Hello world!")
    DOMGlobalScope.alert("haha")
  }
}

@js.native
object DOMGlobalScope extends js.GlobalScope {
  val document: HTMLDocument = js.native
  def alert(message: String): Unit = js.native
}