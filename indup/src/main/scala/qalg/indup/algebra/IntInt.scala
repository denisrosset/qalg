package com.faacets.qalg.indup
package algebra

class IntInt(val encoding: Long) extends AnyVal {
  import IntInt._
  def _1: Int = lowInt(encoding)
  def _2: Int = highInt(encoding)
  def isEmpty: Boolean = false
  def get: IntInt = this
}

object IntInt {
  def unapply(tuple2: IntInt): IntInt = tuple2
  def apply(_1: Int, _2: Int): IntInt = new IntInt(encode(_1, _2))
  @inline def leftMask: Long  = (0xFFFFFFFFL) << 32
  @inline def rightMask: Long = 0xFFFFFFFFL
  @inline def lowInt(l: Long): Int = (l & rightMask).toInt
  @inline def highInt(l: Long): Int = ((l & leftMask) >> 32).toInt
  @inline def encode(low: Int, high: Int): Long = (low.toLong & rightMask) + (high.toLong << 32)
}
