package com.kdr2.scala0.spec0

class Generator[T](var data: List[T]) {
  def g() = for (x <- data) yield x;
}
