package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

/* Implementation trait for dense structures. */
trait Dense1[T1, +R1, @sp A] extends Any with Index1[T1, R1, A] {
  def offsetHead(t: T1): OptOffset =
    if (size(t) == 0) NoOffset else Offset(0)

  def offsetNext(t: T1, offset: Offset): OptOffset =
    if (offset.o == size(t) - 1) NoOffset else Offset(offset.o + 1)

  def offsetValue(t: T1, offset: Offset): A = apply(t, offset.o.toInt)

  def offsetX(t: T1, offset: Offset): Int = offset.o.toInt
  def offsetX0(t: T1, offset: Offset): Int = offset.o.toInt
}
