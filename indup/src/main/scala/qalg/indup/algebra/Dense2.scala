package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

import spire.syntax.cfor._

/* Implementation trait for dense structures. */
trait Dense2[T2, +R2, +R1, @sp A] extends Any with Index2[T2, R2, R1, A] {
  def offsetHead(t: T2): OptOffset =
    if (size0(t) == 0 || size1(t) == 0) NoOffset else Offset(0L)

  def offsetNext(t: T2, offset: Offset): OptOffset = {
    val i0 = IntInt.lowInt(offset.o)
    val i1 = IntInt.highInt(offset.o)
    if (i0 + 1 >= size0(t)) {
      if (i1 + 1 >= size1(t))
        NoOffset
      else
        Offset(IntInt.encode(0, i1 + 1))
    } else
      Offset(IntInt.encode(i0 + 1, i1))
  }

  def offsetValue(t: T2, offset: Offset): A = {
    apply(t, IntInt.lowInt(offset.o), IntInt.highInt(offset.o))
  }

  def offsetX(t: T2, offset: Offset): IntInt = new IntInt(offset.o)
  def offsetX0(t: T2, offset: Offset): Int = IntInt.lowInt(offset.o)
  def offsetX1(t: T2, offset: Offset): Int = IntInt.highInt(offset.o)
}
