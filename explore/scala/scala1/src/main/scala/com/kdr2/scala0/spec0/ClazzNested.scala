package com.kdr2.scala0.spec0

class ClazzNested {

  class InnerNode {
    def printNodeS(n: InnerNode) = println(n);
    def printNodeT(n: ClazzNested#InnerNode) = println(n);
  }

  def createNode: InnerNode = new InnerNode;
  def printNodeS(n: InnerNode) = println(n);
}
