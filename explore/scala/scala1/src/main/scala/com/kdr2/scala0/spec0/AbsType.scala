package com.kdr2.scala0.spec0

import scala.language.postfixOps

abstract class AbsType {
  type T;
  var e: T;
  def p = println(e);
}

object ATRunner {
  def run = {
    val o = new AbsType {
      type T = Int;
      var e = 1;
    };
    o p;
  }
}
