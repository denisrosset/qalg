package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedVecMutable[V, @sp(Double, Long) A, J] extends Any
    with Converted[A, J]
    with VecMutable[V, A] {
  def source: VecMutable[V, J]
  def update(v: V, k: Int, a: A): Unit = source.update(v, k, aToJ(a))
  override def update(v: V, at: At1, a: A): Unit = source.update(v, at, aToJ(a))
  override def update(v: V, at: ::.type, a: A): Unit = source.update(v, at, aToJ(a))
  def copy(v: V): V = source.copy(v)
}
