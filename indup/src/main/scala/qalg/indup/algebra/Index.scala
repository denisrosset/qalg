package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

trait Index[T, I, @sp A] extends Any with Size[T, I] {
  implicit def zeroA: Zero[A]

  def apply(t: T, i: I): A
  /** Returns the offset of the first element, or an invalid offset if the
    * structure is empty . */
  def offsetHead(t: T): OptOffset
  /** Returns the next offset in the sequence of offsets (not necessarily sorted
    * by indices). */
  def offsetNext(t: T, offset: Offset): OptOffset
  /** Returns the value pointed by the offset. */
  def offsetValue(t: T, offset: Offset): A
  /** Returns the index pointed by the offset. */
  def offsetX(t: T, offset: Offset): I
}
