package com.kdr2.scala0

import com.kdr2.scala0.spec0.{ ClazzNested, Function }
import com.kdr2.scala0.spec0.Generator
import com.kdr2.scala0.spec0.Views

object Runner {
  def main(args: Array[String]) {

    //cn1.printNodeS(i2); //wrong
    //testFunctionClazz;
    //testGenerator;
    Views.test;
  }

  def testInnerClazz() = {
    val cn1 = new ClazzNested;
    val cn2 = new ClazzNested;
    val i1 = cn1.createNode;
    val i2 = cn2.createNode;
    println(i1.isInstanceOf[cn1.InnerNode]);
    println(i1.isInstanceOf[cn2.InnerNode]);
    println(i1.isInstanceOf[ClazzNested#InnerNode]);
    i1.printNodeS(i1);
    i1.printNodeT(i2);
    cn1.printNodeS(i1);
  }

  def testFunctionClazz() = {
    val f = new Function();
    f(1);
  }

  def testGenerator() = {
    val d = new Generator(2 :: 1 :: Nil);
    for (x <- d.g) println(x);
  }
}
