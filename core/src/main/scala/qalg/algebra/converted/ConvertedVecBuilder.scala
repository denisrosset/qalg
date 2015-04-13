package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

trait ConvertedVecBuilder[V, @sp(Double, Long) A, J] extends Any
    with ConvertedVec[V, A, J]
    with VecBuilder[V, A] {
  def source: VecBuilder[V, J]

  def tabulate(n: Int)(f: Int => A): V = source.tabulate(n)(k => aToJ(f(k)))
  override def apply(v: V, at: At1): V = source(v, at)
  override def apply(v: V, at: ::.type): V = source(v, at)
}
