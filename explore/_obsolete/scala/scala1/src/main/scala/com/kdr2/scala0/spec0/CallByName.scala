package com.kdr2.scala0.spec0

class CallByName {

  def cbn(code: => Unit) = {
    var var_a = 1
    println("in cbn:" + var_a);
    code
    code
  }

}

object CallByName {

  def call() = {
    val co = new CallByName;
    var var_a = 20
    co.cbn {
      var_a += 1;
      println(var_a)
    };
    co.cbn {
      var_a += 1;
      println(var_a)
    };
  }

}
