package com.faacets.qalg.indup
package algebra

import scala.{specialized => sp}

trait SparseNext0[T, I, @sp A] extends Any with Index[T, I, A] {
  /** Returns the next offset, iterating first in the first direction. */
  def offsetNext0(t: T, offset: Offset): OptOffset
}

trait SparseNext1[T, I, @sp A] extends Any with Index[T, I, A] {
  /** Returns the next offset, iterating first in the second direction. */
  def offsetNext1(t: T, offset: Offset): OptOffset
}
