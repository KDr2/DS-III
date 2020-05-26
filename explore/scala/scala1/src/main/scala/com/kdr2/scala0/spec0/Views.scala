package com.kdr2.scala0.spec0

import scala.language.implicitConversions;

object Views {
  class T1(val i: Int) {
  }

  class T2(val f: Float) {
  }

  class T3(var f: Float) {
  }

  object T2 {
    implicit def t1tot2(t: T1): T2 = {
      println("T1->T2");
      return new T2(t.i.toFloat);
    }
  }

  def p(t: T2) = println("T2: " + t.f);

  def test = {
    val t1 = new T1(100);
    p(t1);
    //t1.i = 2;
    val t3 = new T3(10F);
    t3.f = 10.1F;
    p(t1)
  }
}
