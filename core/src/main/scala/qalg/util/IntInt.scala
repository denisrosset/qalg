package com.faacets.qalg
package util

class IntInt(val encoding: Long) extends AnyVal {
  import IntInt._
  def _1: Int = (encoding & rightMask).toInt
  def _2: Int = ((encoding & leftMask) >> 32).toInt
  def isEmpty: Boolean = false
  def get: IntInt = this
}

object IntInt {
  def unapply(tuple2: IntInt): IntInt = tuple2
  def apply(_1: Int, _2: Int): IntInt = new IntInt(((_1.toLong) & rightMask) + (_2.toLong << 32))
  @inline def leftMask: Long  = (0xFFFFFFFFL) << 32
  @inline def rightMask: Long = 0xFFFFFFFFL
}
