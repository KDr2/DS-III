package com.kdr2.scala0

import com.kdr2.scala0.spec0.{ATRunner, CallByName}

object Main {

  def main(args: Array[String]): Unit = {
    println("Another Main Function");
    CallByName.call();
    ATRunner.run;
    spec0.say("message in package object");
  }

}


object MainHello {

  def main(args: Array[String]): Unit = {
    println("Hello, Main!");
  }

}
