package com.faacets.qalg
package util

/** Map of disjoint intervals to values. */ 
sealed trait RangeMap[B] extends Map[Int, B] {
  def range: Range
  def iterator: Iterator[(Int, B)] = range.iterator.map( k => (k, apply(k)) )
  def get(k: Int): Option[B]
  def +[B1 >: B](kv: (Int, B1)): Map[Int, B1] = iterator.toMap + kv
  def -(key: Int): Map[Int, B] = iterator.toMap - key
}

object RangeMap {
  protected class Branch[B](val left: RangeMap[B], val right: RangeMap[B]) extends RangeMap[B] {
    require(left.range.last < right.range.start)
    val range = left.range.start to right.range.last
    def get(k: Int): Option[B] =
      if (k <= left.range.last) left.get(k) else right.get(k)
  }
  protected class Leaf[B](val range: Range, val b: B) extends RangeMap[B] {
    def get(k: Int) =
      if (range.contains(k)) Some(b) else None
  }
  protected def build[B](sortedElems: Seq[(Range, B)]): RangeMap[B] = sortedElems match {
    case Seq() => throw new IllegalArgumentException("Cannot construct empty RangeMap.")
    case Seq((range, b)) => new Leaf(range, b)
    case _ =>
      val mid = scala.math.min(1, sortedElems.size / 2)
      val (leftElems, rightElems) = sortedElems.splitAt(mid)
      new Branch(build(leftElems), build(rightElems))
  }

  def apply[B](elems: (Range, B)*): RangeMap[B] = build(elems.sortBy(_._1.start))
}
