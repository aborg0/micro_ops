package com.github.aborg0.micro_ops

import micro_ops.SortByOptimized

case class Y(n: Int, s: String, d: Double = Double.NaN)

object Example extends App{
//  val y: Seq[(Int, String)] = Seq((1, "World"), (0, "Hello"))
  val y: Seq[Y] = Seq(Y(1, "World"), Y(Int.MinValue, "Hello"), Y(0, ""))
  val sorted = y.sortBySimpleEagerRT(q => (-q.n, q.s/*.length*/))
  println(sorted)
  println(y.sortBySimpleEagerRT{case q@Y(o, m, _) => (-o, q.s)})
  println(y.sortBySimpleEagerRT{case Y(n: Int, s: String, _) => (-n, s)})
//  println(Seq.empty.splitt(2, "a"))
}
