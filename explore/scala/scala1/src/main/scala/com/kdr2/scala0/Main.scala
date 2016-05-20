package com.kdr2.scala0

import com.kdr2.scala0.spec0.CallByName
import com.kdr2.scala0.spec0.ATRunner

object Main {

  def main(args: Array[String]): Unit = {
    println("Another Main Function");
    CallByName.call();
    ATRunner.run;
    spec0.say("message in package object");
  }

}
