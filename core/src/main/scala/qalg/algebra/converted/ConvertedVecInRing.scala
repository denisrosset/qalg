package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedVecInRing[V, @sp(Double, Long) A, J] extends Any
    with ConvertedVecBuilder[V, A, J]
    with VecInRing[V, A] {
  def source: VecInRing[V, J]
  override def zero: V = source.zero
  override def id: V = source.id
  override def plus(x: V, y: V): V = source.plus(x, y)
  override def minus(x: V, y: V): V = source.minus(x, y)
  override def negate(v: V): V = source.negate(v)
  override def timesl(a: A, v: V): V = source.timesl(aToJ(a), v)
}
